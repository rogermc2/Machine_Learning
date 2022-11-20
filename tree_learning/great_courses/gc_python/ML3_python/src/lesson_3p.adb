
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
--  with GNATCOLL.VFS;

with ML_Types;

with Classifier_Utilities;
--  with Graphviz_Exporter;
--  with Tree_Printing;

procedure Lesson_3P is
   use ML_Types;
   use ML_Types.String_Package;
   --     use Printing;
   Routine_Name  : constant String := "Lesson_3P ";
   Data          : constant Multi_Output_Data_Record :=
                     Classifier_Utilities.Load_Data ("../../diabetes.csv");
   Feature_Names : constant String_List := Data.Feature_Names;
   X_Data        : constant Value_Data_Lists_2D := Data.Feature_Values;
   --     Labels        : constant Value_Data_Lists_2D := Data.Label_Values;
   Repository    : constant Scripts_Repository := new Scripts_Repository_Record;
   Python        : Python_Scripting := null;
   Errors        : Boolean;
   Num_Samples   : constant Natural := Natural (X_Data.Length);
   Names_Cursor  : String_Package.Cursor := Feature_Names.First;
   Features      : Feature_Names_List;
   --     No_Weights    : Weights.Weight_List :=
   --                       Classifier_Types.Float_Package.Empty_Vector;
   --     Correct       : Natural := 0;
   --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
   --     X_Data_Name    : constant GNATCOLL.VFS.Filesystem_String := "X_Data";
   Fix_Command   : constant String := "fix";
   X_Data_Name    : constant String := "X_Data";
   Labels_Name    : constant String := "Labels";
--     Args           : Arg_List := Create ("fit");
begin
   Put_Line ("Lesson 3P");
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   while Has_Element (Names_Cursor) loop
      Features.Append (Element (Names_Cursor));
      Next (Names_Cursor);
   end loop;
   --     Print_Unbounded_List ("Features", Features);
   --     Print_Value_Data_List ("Features row 16", X_Data.Element (16));
   New_Line;

   Register_Python_Scripting (Repo => Repository, Module => "Test");
   --  Python_Name = "python"
   Python := Python_Scripting (Lookup_Scripting_Language (Repository,
                               Python_Name));
   declare
      theData : Callback_Data'Class := Create (Python, 3);
   begin
      Python.Execute_Command ("import os", Errors => Errors);
      Python.Execute_Command ("cwd = os.getcwd()", Errors => Errors);
      Assert (not Errors, "os.getcwd()");
      Python.Execute_Command (Command => "os.chdir(os.path.join (cwd, 'src'))",
                              Errors => Errors);
      Assert (not Errors, "os.chdir(cwd) failed");
      Python.Execute_Command ("print ('cwd: ', os.getcwd())", Errors => Errors);

      --     Put_Line (Routine_Name & "load.py");
      Python.Execute_File ("load.py", Errors => Errors);
      Assert (not Errors, "Execute_File load.py failed");
      Put_Line (Routine_Name & "load.py file executed");

      --     Python.Execute_Command ("print ('cwd: ', os.getcwd())", Errors => Errors);
      --     Python.Execute_Command (Command => "os.listdir(os.getcwd())",
      --                             Errors => Errors);
      Assert (not Errors, "os.listdir(os.getcwd()) failed");

      Python.Execute_Command
        ("clf = Tree2.classes.DecisionTreeClassifier(max_leaf_nodes = 3)",
         Errors => Errors);
      Assert (not Errors, "set clf failed");
      --  Fit function adjusts weights according to data values so that
      --  better accuracy can be achieved
      Put_Line (Routine_Name & "fit");
--        Append_Argument (Args, X_Data_Name, One_Arg);
--        Append_Argument (Args, Labels_Name, One_Arg);
      Set_Nth_Arg (theData, 1, Fix_Command);
      Set_Nth_Arg (theData, 2, X_Data_Name);
      Set_Nth_Arg (theData, 3, Labels_Name);
      Execute_Command (theData, "fix");
--        Python.Execute_Command ("clf = clf.fit(X_Data, Labels)", Errors => Errors);
      Assert (not Errors, "fit failed");
      --        Classification_Fit (aClassifier, X_Data, Labels, No_Weights);
      --        Tree_Printing.Print_Tree ("Diabetes Tree", aClassifier);
      Put_Line ("----------------------------------------------");
      Put_Line (Routine_Name & "completed.");
      New_Line;

      --     for index in X_Data.First_Index .. X_Data.Last_Index loop
      --          if Base_Decision_Tree.Predict
      --            (aClassifier, X_Data).Element (index).Element (1) =
      --                Labels.Element (index).Element (1) then
      --             Correct := Correct + 1;
      --          end if;
      --     end loop;
      --     Put_Line ("Prediction: " &
      --                 Float'Image (100.0 * Float (Correct) / Float (X_Data.Length)));
      --     New_Line;
   end;  -- declare block
   Python.Destroy;
   Unregister_Python_Scripting (Repository);

   --     Graphviz_Exporter.C_Init
   --       (Exporter, aClassifier.Attributes.Decision_Tree);
   --     Graphviz_Exporter.Export_Graphviz
   --       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
   --        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_3P;
