# Copyright  :  (C) 2021-2022, QBayLogic B.V.,
#                   2022     , Google Inc.,
# License    :  BSD2 (see the file LICENSE)
# Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
#
# Script to parse output generated by Clash to use in the synthesis tool.
#
# TODO: More user documentation

package require json

namespace eval clash {
    variable metadata {}
    variable topEntity

    # Invoke with $topEntityDir set to the path where the manifest file of
    # your top entity is. Read all the metadata generated by Tcl: manifests
    # and Tcl interface scripts, for the top entity and its dependencies.
    proc readMetadata topEntityDir {
        variable metadata
        variable topEntity

        # If we are called multiple times, we will remove the results of
        # earlier invocations.
        set metadata [dict create]
        if {[namespace exists tclIface]} {
            namespace delete tclIface
        }

        set topEntity [ParseManifest $topEntityDir true]
        puts "Top entity is $topEntity"
        return
    }

    # Issue "read_vhdl" / "read_verilog" for all HDL files generated by Clash
    proc readHdl {} {
        variable metadata
        variable topEntity

        CheckMetadataExists
        set libs [dict get $metadata $topEntity dependencies]
        lappend libs $topEntity
        foreach lib $libs {
            foreach hdlFile [dict get $metadata $lib hdlFiles] {
                if {[string match {*.vhdl} $hdlFile]} {
                    read_vhdl -library $lib $hdlFile
                } elseif {[string match {*.v} $hdlFile]} {
                    read_verilog $hdlFile
                } elseif {[string match {*.sv} $hdlFile]} {
                    read_verilog -sv $hdlFile
                } else {
                    error "Error: Unknown extension on HDL file $hdlFile"
                }
            }
        }
        return
    }

    # Issue "read_xdc" for all constraint files generated by Clash for the top
    # entity, and all constraint files managed by the Clash<->Tcl API (not just
    # for the top entity, but for all libraries).
    proc readXdc orders {
        variable metadata
        variable topEntity

        CheckMetadataExists
        foreach order $orders {
            if {$order ni {early normal late}} {
                error "Error: readXdc: Invalid order $order"
            }
        }

        set early {}
        set normal {}
        set late {}
        foreach tclIface [GetAllTclIfaces] {
            if {[subst $${tclIface}::scriptPurpose] ne {readXdc}} {
                continue
            }
            lappend [subst $${tclIface}::order] $tclIface
        }

        if {{early} in $orders} {
            foreach tclIface $early {
                ReadManagedXdc $tclIface
            }
        }
        if {{normal} in $orders} {
            set constraintFiles [dict get $metadata $topEntity constraintFiles]
            foreach constraintFile $constraintFiles {
                read_xdc $constraintFile
            }
            foreach tclIface $normal {
                ReadManagedXdc $tclIface
            }
        }
        if {{late} in $orders} {
            foreach tclIface $late {
                ReadManagedXdc $tclIface
            }
        }
        return
    }

    # Invoke all Clash-generated Tcl interfaces that specify a "createIp"
    # purpose, which will call Vivado's "create_ip" with any additional
    # arguments passed to this function (hint: "createIp -dir yourdir" will
    # create the IP in the subdirectory named "yourdir"). Following that, the
    # IP is configured.
    #
    # Also see "createAndReadIp" below; call it or use its code as
    # inspiration. It is suggested to call it like "createAndReadIp -dir ip"
    # so you keep the files in a separate directory named "ip". The directory
    # will need to exist already.
    proc createIp args {
        CheckMetadataExists
        # Identical names means identical IP, only one run needed even if it
        # occurs in multiple HDL directories.
        set seen {}
        foreach tclIface [GetAllTclIfaces] {
            if {[subst $${tclIface}::scriptPurpose] ne {createIp}} {
                continue
            }
            set ipName [subst $${tclIface}::ipName]
            if {$ipName in $seen} {
                continue
            }
            ${tclIface}::createIp $ipName {*}$args
            lappend seen $ipName
        }
        return $seen
    }

    # Convenience method to create the IP in a temporary in-memory project and
    # then read in the results in non-project mode. Finally, targets are
    # created for the IP.
    #
    # NOTE WELL: since this switches into and out of project mode, it should
    # probably be the very first thing you call in setting up the design.
    proc createAndReadIp args {
        CheckMetadataExists
        create_project -in_memory
        set ips [createIp {*}$args]
        if {$ips ne {}} {
            set ipFiles [get_property IP_FILE [get_ips $ips]]
        }
        close_project
        if {$ips ne {}} {
            read_ip $ipFiles
            set_property GENERATE_SYNTH_CHECKPOINT false [get_files $ipFiles]
            generate_target {synthesis simulation} [get_ips $ips]
        }
        return
    }

    #---------------------------------------------------------------------------
    # Private methods
    #---------------------------------------------------------------------------

    proc CheckMetadataExists {} {
        variable metadata
        if {$metadata eq {}} {
            error "Error: Please invoke clash::readMetadata first."
        }
        return
    }

    proc GetNsVar {ns varName} {
        if {[info vars ${ns}::$varName] eq {}} {
            error "Error: $ns doesn't provide \"$varName\"\
                    variable."
        }
        return [subst $${ns}::$varName]
    }

    proc ParseManifest {entityDir withDeps} {
        variable metadata

        set manC [open [file join $entityDir clash-manifest.json] r]
        set manifest [json::json2dict [read $manC]]
        close $manC
        set lib [dict get $manifest top_component name]
        puts "New top component: $lib"
        # Clash sometimes lists files multiple times, process them only once
        set seen {}
        dict set metadata $lib hdlFiles {}
        dict set metadata $lib constraintFiles {}
        foreach fileEntry [dict get $manifest files] {
            set name [file join $entityDir [dict get $fileEntry name]]
            if {$name in $seen} {
                continue
            }
            lappend seen $name
            if {
                   [string match {*.vhdl} $name]
                || [string match {*.v} $name]
                || [string match {*.sv} $name]
            } then {
                dict with metadata $lib {
                    lappend hdlFiles $name
                }
            } elseif {
                   [string match {*.sdc} $name]
                || [string match {*.xdc} $name]
            } then {
                dict with metadata $lib {
                    lappend constraintFiles $name
                }
            } elseif {[string match {*.tcl} $name]} {
                LoadTclIface $lib $name
            }
        }
        RemoveManagedFiles $lib

        if {!$withDeps} {
            return $lib
        }

        set dependencies {}
        foreach dependency [dict get $manifest dependencies transitive] {
            set dependencyDir [file join [file dirname $entityDir] $dependency]
            lappend dependencies [ParseManifest $dependencyDir false]
        }
        dict set metadata $lib dependencies $dependencies
        return $lib
    }

    # Populate a namespace with a Clash-generated Tcl interface.
    # Namespace is clash::tclIface::${lib}::$baseName
    proc LoadTclIface {lib tclIfaceFile} {
        set baseName [file rootname [file tail $tclIfaceFile]]
        set tclIface [namespace current]::tclIface::${lib}::$baseName
        # Evaluate script code inside temporary throwaway namespace to
        # separate its code from ours and reduce the chance of accidentally
        # corrupting our code.
        namespace eval tmp {}
        set tmp::tclIfaceFile $tclIfaceFile
        set tmp::tclIface $tclIface
        namespace eval tmp {
            # -notrace is a Vivado specific option inhibiting the printing of
            # the script to stdout
            source -notrace $tclIfaceFile
        }
        if {![namespace exists $tclIface]} {
            error "Error: $tclIfaceFile did not create the requested namespace\
                specified by the \$tclIface variable. The Tcl script does not\
                conform to the defined Clash<->Tcl API."
        }
        namespace delete tmp
        VerifyTclIface $tclIface $tclIfaceFile true
        return
    }

    # Verify that the read interface file is strictly something we support.
    proc VerifyTclIface {tclIface tclIfaceFile isRoot} {
        set api [GetNsVar $tclIface api]
        if {$api ne {1}} {
            error "Error: $tclIface doesn't implement an API we support:\
                    api = \"$api\"."
        }
        set purpose [GetNsVar $tclIface scriptPurpose]
        if {$purpose eq {multipleScripts}} {
            if {!$isRoot} {
                error "Error: ${tclIface}::scriptPurpose = \"multipleScripts\",\
                        nested use not allowed."
            }
            if {![namespace exists ${tclIface}::multipleScripts]} {
                error "Error: ${tclIface}::multipleScripts does not exist."
            }
            set children [namespace children ${tclIface}::multipleScripts]
            if {$children eq {}} {
                error "Error: ${tclIface}::multipleScripts doesn't provide any\
                        scripts."
            }
            foreach child $children {
                if {[info vars ${child}::api] ne {}} {
                    error "Error: $child cannot specify api: it is specified by\
                            parent script."
                }
                set ${child}::api $api
                VerifyTclIface $child $tclIfaceFile false
            }

        } elseif {$purpose eq {createIp}} {
            GetNsVar $tclIface ipName
            # In Tcl, you can call procedures with a partial name. So an
            # invocation of "createIp" could call "createIpAlt" if
            # "createIp" did not exist. Let's be strict here to prevent
            # confusion: only accept the exact name "createIp".
            if {[info procs ${tclIface}::createIp] eq {}} {
                error "Error: $tclIface doesn't provide \"createIp\"\
                    procedure."
            }

        } elseif {$purpose eq {readXdc}} {
            set order [GetNsVar $tclIface order]
            if {$order ni {early normal late}} {
                error "Error: ${tclIface}::order bogus value \"$order\"."
            }
            set usedIn [GetNsVar $tclIface usedIn]
            foreach stage $usedIn {
                if {$stage ni {synthesis implementation}} {
                    error "Error: ${tclIface}::usedIn bogus value \"$stage\"."
                }
            }
            set xdcFile [GetNsVar $tclIface xdcFile]
            # "file join" also correctly handles an absolute $xdcFile
            set resolvedFile [file join [file dirname $tclIfaceFile] $xdcFile]
            # "file isfile" also positively matches a symlink to a regular
            # file
            if {![file isfile $resolvedFile]} {
                error "Error: ${tclIface}::xdcFile = \"$xdcFile\" does not\
                        refer to an existing file."
            }
            set ${tclIface}::xdcFile $resolvedFile

        } else {
            error "Error: ${tclIface}::scriptPurpose bogus value\
                    \"$purpose\"."
        }
        return
    }

    # Remove constraint files that are managed by a Tcl interface script from
    # the list of constraint files for the library.
    proc RemoveManagedFiles lib {
        variable metadata

        foreach tclIface [GetAllTclIfaces $lib] {
            if {[subst $${tclIface}::scriptPurpose] ne {readXdc}} {
                continue
            }
            set xdcFile [subst $${tclIface}::xdcFile]
            set constraintFiles [dict get $metadata $lib constraintFiles]
            set filtered [lsearch -all -inline -exact -not $constraintFiles\
                    $xdcFile]
            dict set metadata $lib constraintFiles $filtered
        }
        return
    }

    # Return all the Tcl interface namespaces
    #
    # They can optionally be filtered by specifying a list of libraries to
    # consider or a list of interface names to consider. For example, if these
    # two interfaces exist:
    #   - ::clash::tclIface::libA::ifaceX
    #   - ::clash::tclIface::libB::ifaceX
    #
    # Then "GetAllTclIfaces libA" would only return the first, but
    # "GetAllTclIfaces {} ifaceX" would return both since the libraries are not
    # filtered (empty list) but the interface names are (and they both match).
    proc GetAllTclIfaces {{libs {}} {ifaces {}}} {
        if {![namespace exists tclIface]} {
            # There are no scripts
            return
        }

        if {$libs eq {}} {
            set walkLibs [namespace children tclIface]
        } else {
            set walkLibs {}
            foreach lib $libs {
                if {[namespace exists tclIface::$lib]} {
                    lappend walkLibs tclIface::$lib
                }
            }
        }

        set tclIfaces {}
        foreach libNs $walkLibs {
            foreach tclIface [namespace children $libNs] {
                if {$ifaces ne {} && [namespace tail $tclIface] ni $ifaces} {
                    continue
                }
                if {[subst $${tclIface}::scriptPurpose] ne {multipleScripts}} {
                    lappend tclIfaces $tclIface
                    continue
                }
                set children [namespace children ${tclIface}::multipleScripts]
                set tclIfaces [concat $tclIfaces $children]
            }
        }
        return $tclIfaces
    }

    proc ReadManagedXdc tclIface {
        set xdcFile [subst $${tclIface}::xdcFile]
        if {[info vars ${tclIface}::scopeRef] ne {}} {
            set scopeRef [list -ref [subst $${tclIface}::scopeRef]]
        } else {
            set scopeRef {}
        }
        read_xdc {*}$scopeRef $xdcFile
        set_property USED_IN [subst $${tclIface}::usedIn] [get_files $xdcFile]
        return
    }
}