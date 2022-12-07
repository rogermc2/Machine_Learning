
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types; use ML_Types;

with Classifier_Utilities;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Printing;
with Python;

procedure Lesson_3P2 is
   use ML_Types.String_Package;
   Routine_Name  : constant String := "Lesson_3P2";
   Data          : constant Multi_Output_Data_Record :=
                     Classifier_Utilities.Load_Data ("../diabetes.csv");
   Feature_Names : constant String_List := Data.Feature_Names;
   X_Data_List   : constant Value_Data_Lists_2D := Data.Feature_Values;
   Labels_List   : constant Value_Data_Lists_2D := Data.Label_Values;
   Num_Samples   : constant Natural := Natural (X_Data_List.Length);
   X_Data        : constant Integer_Matrix := To_Integer_Matrix (X_Data_List);
   Labels        : constant Integer_Matrix := To_Integer_Matrix (Labels_List);
   --     Base          : Python.Module;
   Classes       : Python.Module;
   Names_Cursor  : String_Package.Cursor := Feature_Names.First;
   Features      : Feature_Names_List;
   --     No_Weights    : Weights.Weight_List :=
   --                       Classifier_Types.Float_Package.Empty_Vector;
   --     Correct       : Natural := 0;
   --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
begin
   Put_Line (Routine_Name);
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   while Has_Element (Names_Cursor) loop
      Features.Append (Element (Names_Cursor));
      Next (Names_Cursor);
   end loop;

--     Printing.Print_Unbounded_List ("Features", Features);
--     Printing.Print_Value_Data_List ("Features row 16", X_Data_List.Element (16));
   New_Line;

   Python.Initialize;
   Put_Line ("Lesson 3 python initialized");
   Python.Execute_String ("import tree");
   Put_Line ("Lesson 3 tree imported");
   Python.Execute_String ("from tree import base");
   Put_Line ("Lesson 3 base imported");
   Classes := Python.Import_File ("tree.classes");
   Put_Line ("Lesson 3 Classes loaded");
   Python.Execute_String ("from tree import classes");
   Put_Line ("Lesson 3 modules loaded");

   Python.Execute_String
     ("clf = tree.classes.DecisionTreeClassifier(max_leaf_nodes = 3)");
   --  Fit function adjusts weights according to data values so that
   --  better accuracy can be achieved
   Put_Line ("Lesson 3 fit");
   --     Python.Execute_String ("clf.fit(X_Data, Labels)");
   Python.Call (M => Classes, Function_Name => "fit",
                A => X_Data, B             => Labels);
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

   --     Python.Close_Module (Base);
   --     Python.Close_Module (Classes);
   Python.Finalize;

   --     Graphviz_Exporter.C_Init
   --       (Exporter, aClassifier.Attributes.Decision_Tree);
   --     Graphviz_Exporter.Export_Graphviz
   --       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
   --        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_3P2;
