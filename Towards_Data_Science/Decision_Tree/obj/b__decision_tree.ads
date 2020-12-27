pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2020 (20200429-84)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_decision_tree" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#0d7edef5#;
   pragma Export (C, u00001, "decision_treeB");
   u00002 : constant Version_32 := 16#67c8d842#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#6dba5b66#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#ba5b22d1#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#f540b206#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#35e1815f#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#cfec26ee#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#776ed810#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#632dfee6#;
   pragma Export (C, u00012, "system__secondary_stackB");
   u00013 : constant Version_32 := 16#f9f0587f#;
   pragma Export (C, u00013, "system__secondary_stackS");
   u00014 : constant Version_32 := 16#896564a3#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#4f09ab30#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#ce3e0e21#;
   pragma Export (C, u00018, "system__soft_links__initializeB");
   u00019 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00019, "system__soft_links__initializeS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#37322c0b#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#5c74e542#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#1416bc8d#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#46491211#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#ae2d2db5#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00040, "system__address_imageB");
   u00041 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00041, "system__address_imageS");
   u00042 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00042, "system__wch_conB");
   u00043 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00043, "system__wch_conS");
   u00044 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00044, "system__wch_stwB");
   u00045 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00045, "system__wch_stwS");
   u00046 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00046, "system__wch_cnvB");
   u00047 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00047, "system__wch_cnvS");
   u00048 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00048, "interfacesS");
   u00049 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00049, "system__wch_jisB");
   u00050 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00050, "system__wch_jisS");
   u00051 : constant Version_32 := 16#f9576a72#;
   pragma Export (C, u00051, "ada__tagsB");
   u00052 : constant Version_32 := 16#b6661f55#;
   pragma Export (C, u00052, "ada__tagsS");
   u00053 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00053, "system__htableB");
   u00054 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00054, "system__htableS");
   u00055 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00055, "system__string_hashB");
   u00056 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00056, "system__string_hashS");
   u00057 : constant Version_32 := 16#56941de9#;
   pragma Export (C, u00057, "system__unsigned_typesS");
   u00058 : constant Version_32 := 16#d2ae2792#;
   pragma Export (C, u00058, "system__val_lluB");
   u00059 : constant Version_32 := 16#3b5a900b#;
   pragma Export (C, u00059, "system__val_lluS");
   u00060 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00060, "system__val_utilB");
   u00061 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00061, "system__val_utilS");
   u00062 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00062, "system__case_utilB");
   u00063 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00063, "system__case_utilS");
   u00064 : constant Version_32 := 16#f4e097a7#;
   pragma Export (C, u00064, "ada__text_ioB");
   u00065 : constant Version_32 := 16#3913d0d6#;
   pragma Export (C, u00065, "ada__text_ioS");
   u00066 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00066, "ada__streamsB");
   u00067 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00067, "ada__streamsS");
   u00068 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00069, "interfaces__c_streamsB");
   u00070 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00070, "interfaces__c_streamsS");
   u00071 : constant Version_32 := 16#41b27041#;
   pragma Export (C, u00071, "system__crtlS");
   u00072 : constant Version_32 := 16#ec9c64c3#;
   pragma Export (C, u00072, "system__file_ioB");
   u00073 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00073, "system__file_ioS");
   u00074 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00074, "ada__finalizationS");
   u00075 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00075, "system__finalization_rootB");
   u00076 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00076, "system__finalization_rootS");
   u00077 : constant Version_32 := 16#5f851299#;
   pragma Export (C, u00077, "system__os_libB");
   u00078 : constant Version_32 := 16#d872da39#;
   pragma Export (C, u00078, "system__os_libS");
   u00079 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00079, "system__stringsB");
   u00080 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00080, "system__stringsS");
   u00081 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00081, "system__file_control_blockS");
   u00082 : constant Version_32 := 16#3d68b239#;
   pragma Export (C, u00082, "builderB");
   u00083 : constant Version_32 := 16#d713a074#;
   pragma Export (C, u00083, "builderS");
   u00084 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00084, "ada__containersS");
   u00085 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00085, "system__concat_2B");
   u00086 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00086, "system__concat_2S");
   u00087 : constant Version_32 := 16#b31a5821#;
   pragma Export (C, u00087, "system__img_enum_newB");
   u00088 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00088, "system__img_enum_newS");
   u00089 : constant Version_32 := 16#d5d8c501#;
   pragma Export (C, u00089, "system__storage_pools__subpoolsB");
   u00090 : constant Version_32 := 16#e136d7bf#;
   pragma Export (C, u00090, "system__storage_pools__subpoolsS");
   u00091 : constant Version_32 := 16#57674f80#;
   pragma Export (C, u00091, "system__finalization_mastersB");
   u00092 : constant Version_32 := 16#0b3c2f2b#;
   pragma Export (C, u00092, "system__finalization_mastersS");
   u00093 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00093, "system__img_boolB");
   u00094 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00094, "system__img_boolS");
   u00095 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00095, "system__ioB");
   u00096 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00096, "system__ioS");
   u00097 : constant Version_32 := 16#35d6ef80#;
   pragma Export (C, u00097, "system__storage_poolsB");
   u00098 : constant Version_32 := 16#732d884c#;
   pragma Export (C, u00098, "system__storage_poolsS");
   u00099 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00099, "system__storage_pools__subpools__finalizationB");
   u00100 : constant Version_32 := 16#8bd8fdc9#;
   pragma Export (C, u00100, "system__storage_pools__subpools__finalizationS");
   u00101 : constant Version_32 := 16#c89f77d5#;
   pragma Export (C, u00101, "ada__containers__helpersB");
   u00102 : constant Version_32 := 16#4adfc5eb#;
   pragma Export (C, u00102, "ada__containers__helpersS");
   u00103 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00103, "system__atomic_countersB");
   u00104 : constant Version_32 := 16#bc074276#;
   pragma Export (C, u00104, "system__atomic_countersS");
   u00105 : constant Version_32 := 16#be1811d3#;
   pragma Export (C, u00105, "supportB");
   u00106 : constant Version_32 := 16#1d1e2b97#;
   pragma Export (C, u00106, "supportS");
   u00107 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00107, "system__concat_5B");
   u00108 : constant Version_32 := 16#8f052cd5#;
   pragma Export (C, u00108, "system__concat_5S");
   u00109 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00109, "system__concat_4B");
   u00110 : constant Version_32 := 16#763f44db#;
   pragma Export (C, u00110, "system__concat_4S");
   u00111 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00111, "system__concat_3B");
   u00112 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00112, "system__concat_3S");
   u00113 : constant Version_32 := 16#46899fd1#;
   pragma Export (C, u00113, "system__concat_7B");
   u00114 : constant Version_32 := 16#f49c34e4#;
   pragma Export (C, u00114, "system__concat_7S");
   u00115 : constant Version_32 := 16#a83b7c85#;
   pragma Export (C, u00115, "system__concat_6B");
   u00116 : constant Version_32 := 16#da9c4249#;
   pragma Export (C, u00116, "system__concat_6S");
   u00117 : constant Version_32 := 16#8225628b#;
   pragma Export (C, u00117, "ada__containers__red_black_treesS");
   u00118 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00118, "ada__stringsS");
   u00119 : constant Version_32 := 16#c6ca4532#;
   pragma Export (C, u00119, "ada__strings__unboundedB");
   u00120 : constant Version_32 := 16#6552cb60#;
   pragma Export (C, u00120, "ada__strings__unboundedS");
   u00121 : constant Version_32 := 16#2eb48a6d#;
   pragma Export (C, u00121, "ada__strings__searchB");
   u00122 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00122, "ada__strings__searchS");
   u00123 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00123, "ada__strings__mapsB");
   u00124 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00124, "ada__strings__mapsS");
   u00125 : constant Version_32 := 16#7ca1465f#;
   pragma Export (C, u00125, "system__bit_opsB");
   u00126 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00126, "system__bit_opsS");
   u00127 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00127, "ada__charactersS");
   u00128 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00128, "ada__characters__latin_1S");
   u00129 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00129, "system__compare_array_unsigned_8B");
   u00130 : constant Version_32 := 16#a1581e76#;
   pragma Export (C, u00130, "system__compare_array_unsigned_8S");
   u00131 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00131, "system__address_operationsB");
   u00132 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00132, "system__address_operationsS");
   u00133 : constant Version_32 := 16#5252521d#;
   pragma Export (C, u00133, "system__stream_attributesB");
   u00134 : constant Version_32 := 16#d573b948#;
   pragma Export (C, u00134, "system__stream_attributesS");
   u00135 : constant Version_32 := 16#3e25f63c#;
   pragma Export (C, u00135, "system__stream_attributes__xdrB");
   u00136 : constant Version_32 := 16#2f60cd1f#;
   pragma Export (C, u00136, "system__stream_attributes__xdrS");
   u00137 : constant Version_32 := 16#502e73ef#;
   pragma Export (C, u00137, "system__fat_fltS");
   u00138 : constant Version_32 := 16#761c7ae2#;
   pragma Export (C, u00138, "system__fat_lfltS");
   u00139 : constant Version_32 := 16#0cccd408#;
   pragma Export (C, u00139, "system__fat_llfS");
   u00140 : constant Version_32 := 16#a368b3ae#;
   pragma Export (C, u00140, "system__fat_sfltS");
   u00141 : constant Version_32 := 16#021224f8#;
   pragma Export (C, u00141, "system__pool_globalB");
   u00142 : constant Version_32 := 16#29da5924#;
   pragma Export (C, u00142, "system__pool_globalS");
   u00143 : constant Version_32 := 16#eca5ecae#;
   pragma Export (C, u00143, "system__memoryB");
   u00144 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00144, "system__memoryS");
   u00145 : constant Version_32 := 16#c809f030#;
   pragma Export (C, u00145, "system__taskingB");
   u00146 : constant Version_32 := 16#848e268f#;
   pragma Export (C, u00146, "system__taskingS");
   u00147 : constant Version_32 := 16#fde20231#;
   pragma Export (C, u00147, "system__task_primitivesS");
   u00148 : constant Version_32 := 16#352452d1#;
   pragma Export (C, u00148, "system__os_interfaceB");
   u00149 : constant Version_32 := 16#863b4995#;
   pragma Export (C, u00149, "system__os_interfaceS");
   u00150 : constant Version_32 := 16#e49bce3e#;
   pragma Export (C, u00150, "interfaces__cB");
   u00151 : constant Version_32 := 16#dbc36ce0#;
   pragma Export (C, u00151, "interfaces__cS");
   u00152 : constant Version_32 := 16#f6a874ae#;
   pragma Export (C, u00152, "interfaces__c__extensionsS");
   u00153 : constant Version_32 := 16#8788e4d2#;
   pragma Export (C, u00153, "system__os_constantsS");
   u00154 : constant Version_32 := 16#7e77a953#;
   pragma Export (C, u00154, "system__task_primitives__operationsB");
   u00155 : constant Version_32 := 16#54ffbebe#;
   pragma Export (C, u00155, "system__task_primitives__operationsS");
   u00156 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00156, "system__interrupt_managementB");
   u00157 : constant Version_32 := 16#887626f9#;
   pragma Export (C, u00157, "system__interrupt_managementS");
   u00158 : constant Version_32 := 16#64507e17#;
   pragma Export (C, u00158, "system__multiprocessorsB");
   u00159 : constant Version_32 := 16#30f7f088#;
   pragma Export (C, u00159, "system__multiprocessorsS");
   u00160 : constant Version_32 := 16#2b2125d3#;
   pragma Export (C, u00160, "system__os_primitivesB");
   u00161 : constant Version_32 := 16#0fa60a0d#;
   pragma Export (C, u00161, "system__os_primitivesS");
   u00162 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00162, "system__task_infoB");
   u00163 : constant Version_32 := 16#8841d2fa#;
   pragma Export (C, u00163, "system__task_infoS");
   u00164 : constant Version_32 := 16#83ab03df#;
   pragma Export (C, u00164, "system__tasking__debugB");
   u00165 : constant Version_32 := 16#de1ac8b1#;
   pragma Export (C, u00165, "system__tasking__debugS");
   u00166 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00166, "system__img_lliB");
   u00167 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00167, "system__img_lliS");
   u00168 : constant Version_32 := 16#617d5887#;
   pragma Export (C, u00168, "system__stack_usageB");
   u00169 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00169, "system__stack_usageS");
   u00170 : constant Version_32 := 16#8f828546#;
   pragma Export (C, u00170, "system__img_realB");
   u00171 : constant Version_32 := 16#97c0f869#;
   pragma Export (C, u00171, "system__img_realS");
   u00172 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00172, "system__float_controlB");
   u00173 : constant Version_32 := 16#e8a72cc7#;
   pragma Export (C, u00173, "system__float_controlS");
   u00174 : constant Version_32 := 16#54da27e6#;
   pragma Export (C, u00174, "system__img_lluB");
   u00175 : constant Version_32 := 16#1f5d1d2a#;
   pragma Export (C, u00175, "system__img_lluS");
   u00176 : constant Version_32 := 16#8631cc2e#;
   pragma Export (C, u00176, "system__img_unsB");
   u00177 : constant Version_32 := 16#c960211e#;
   pragma Export (C, u00177, "system__img_unsS");
   u00178 : constant Version_32 := 16#582b098c#;
   pragma Export (C, u00178, "system__powten_tableS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.memory%s
   --  system.memory%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  interfaces.c.extensions%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.fat_sflt%s
   --  system.img_real%s
   --  system.img_real%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.red_black_trees%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes.xdr%s
   --  system.stream_attributes.xdr%b
   --  system.stream_attributes%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  support%s
   --  support%b
   --  builder%s
   --  builder%b
   --  decision_tree%b
   --  END ELABORATION ORDER

end ada_main;
