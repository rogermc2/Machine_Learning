
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with Printing;
with Python;
with Tree;
with Weights;

procedure Lesson_3 is
   use ML_Types;
   use ML_Types.String_Package;
   use Decision_Tree_Classification;
   use Printing;
   Routine_Name  : constant String := "Lesson_3";
   Min_Split     : constant String := "2";
   Data          : constant Multi_Output_Data_Record :=
                     Classifier_Utilities.Load_Data ("../diabetes.csv");
   Feature_Names : constant String_List := Data.Feature_Names;
   X_Data        : constant Value_Data_Lists_2D := Data.Feature_Values;
   Num_Samples   : constant Natural := Natural (X_Data.Length);
   Module        : Python.Module;
   Classes       : Python.Module;
   Names_Cursor  : String_Package.Cursor := Feature_Names.First;
   Features      : Feature_Names_List;
   aClassifier   : Base_Decision_Tree.Classifier
     (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   Labels        : Value_Data_Lists_2D;
   No_Weights    : Weights.Weight_List :=
                     Classifier_Types.Float_Package.Empty_Vector;
   Correct       : Natural := 0;
   Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
begin
   Put_Line ("Lesson 3");
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   --  Labels is 2D list num outputs x num samples
   Labels := Data.Label_Values;
   while Has_Element (Names_Cursor) loop
      Features.Append (Element (Names_Cursor));
      Next (Names_Cursor);
   end loop;
   Print_Unbounded_List ("Features", Features);
   Print_Value_Data_List ("Features row 16", X_Data.Element (16));
   New_Line;

   Python.Initialize;
   Module := Python.Import_File ("python_module");
   Classes := Python.Import_File ("_classes.py");

   C_Init (aClassifier, Min_Split, Criterion.Gini_Criteria,
           Max_Leaf_Nodes => 3);

   --  Fit function adjusts weights according to data values so that
   --  better accuracy can be achieved
   Classification_Fit (aClassifier, X_Data, Labels, No_Weights);
   Printing.Print_Tree ("Diabetes Tree", aClassifier);
   Put_Line ("----------------------------------------------");
   New_Line;

   for index in X_Data.First_Index .. X_Data.Last_Index loop
        if Base_Decision_Tree.Predict
          (aClassifier, X_Data).Element (index).Element (1) =
              Labels.Element (index).Element (1) then
           Correct := Correct + 1;
        end if;
   end loop;
   Put_Line ("Prediction: " &
               Float'Image (100.0 * Float (Correct) / Float (X_Data.Length)));
   New_Line;

   Python.Close_Module (Module);
   Python.Close_Module (Classes);
   Python.Finalize;

   Graphviz_Exporter.C_Init
     (Exporter, aClassifier.Attributes.Decision_Tree);
   Graphviz_Exporter.Export_Graphviz
     (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
      Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_3;