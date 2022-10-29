
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with ML_Types;

with Classifier_Utilities;
--  with Graphviz_Exporter;
with Printing;

procedure Lesson_3 is
   use ML_Types;
   use ML_Types.String_Package;
   use Printing;
   Routine_Name  : constant String := "Lesson_3";
   Data          : constant Multi_Output_Data_Record :=
                     Classifier_Utilities.Load_Data ("../diabetes.csv");
   Feature_Names : constant String_List := Data.Feature_Names;
   X_Data        : constant Value_Data_Lists_2D := Data.Feature_Values;
   --     Labels        : constant Value_Data_Lists_2D := Data.Label_Values;
   Repository    : Scripts_Repository := null;
   Python        : Python_Scripting := null;
   Errors        : Boolean;
   Num_Samples   : constant Natural := Natural (X_Data.Length);
   Names_Cursor  : String_Package.Cursor := Feature_Names.First;
   Features      : Feature_Names_List;
   --     No_Weights    : Weights.Weight_List :=
   --                       Classifier_Types.Float_Package.Empty_Vector;
   --     Correct       : Natural := 0;
   --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
begin
   Put_Line ("Lesson 3");
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   while Has_Element (Names_Cursor) loop
      Features.Append (Element (Names_Cursor));
      Next (Names_Cursor);
   end loop;
   Print_Unbounded_List ("Features", Features);
   Print_Value_Data_List ("Features row 16", X_Data.Element (16));
   New_Line;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting (Repo => Repository, Module => "Test");
   --  Python_Name = "python"
   Python := Python_Scripting (Lookup_Scripting_Language (Repository,
                               Python_Name));
   Python.Execute_Command ("import os", Errors => Errors);
   Python.Execute_Command ("cwd = os.getcwd()", Errors => Errors);
   Assert (not Errors, "os.getcwd()");
   Python.Execute_Command ("print ('cwd: ', cwd)", Errors => Errors);
   --     Python.Execute_Command ("from pathlib import Path", Errors => Errors);
   --     Assert (not Errors, "import Path failed");
   --     Python.Execute_Command ("cwd = Path(cwd).parent / ('..')",
   --                             Errors => Errors);
   --     Python.Execute_Command ("cwd = '/Ada_Projects/machine_learning/tree_learning'",
   --                             Errors => Errors);
   Python.Execute_Command (Command => "os.chdir(os.path.join (cwd, 'src'))", Errors => Errors);
   Assert (not Errors, "os.chdir(cwd) failed");
   Python.Execute_Command ("cwd = os.getcwd()", Errors => Errors);
   Python.Execute_Command ("print ('cwd: ', os.getcwd())", Errors => Errors);
   Python.Execute_Command (Command => "os.listdir(os.getcwd())",
                           Errors => Errors);
   Assert (not Errors, "os.listdir(os.getcwd()) failed");

   Python.Execute_Command ("import Tree2", Errors => Errors);
   Put_Line ("Errors: " & Boolean'Image (Errors));
   Assert (not Errors, "import Tree2 failed");
   Put_Line ("Lesson 3 Tree imported");
   Python.Execute_Command (Command => "cwd = os.path.join (cwd, 'Tree2')",
                           Errors => Errors);
   Python.Execute_Command (Command => "os.chdir(cwd)", Errors => Errors);
   Python.Execute_Command (Command => "os.listdir(os.getcwd())", Errors => Errors);
   Python.Execute_Command ("print ('cwd: ', cwd)", Errors => Errors);
   Python.Execute_Command ("import base", Errors => Errors);
   Python.Execute_Command ("import classes", Errors => Errors);

   Python.Execute_Command
     ("clf = tree.DecisionTreeClassifier(max_leaf_nodes = 3)",
      Errors => Errors);
   --  Fit function adjusts weights according to data values so that
   --  better accuracy can be achieved
   Put_Line ("Lesson 3 fit");
   Python.Execute_Command ("clf = fit(X_Data, Labels)", Errors => Errors);
   --     Classification_Fit (aClassifier, X_Data, Labels, No_Weights);
   --     Printing.Print_Tree ("Diabetes Tree", aClassifier);
   Put_Line ("----------------------------------------------");
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

   Python.Destroy;
   Unregister_Python_Scripting (Repository);

   --     Graphviz_Exporter.C_Init
   --       (Exporter, aClassifier.Attributes.Decision_Tree);
   --     Graphviz_Exporter.Export_Graphviz
   --       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
   --        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_3;
