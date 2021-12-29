
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
with Tree;
with Weights;

procedure Lesson_3 is
   use ML_Types;
   use Decision_Tree_Classification;
   Routine_Name   : constant String := "Lesson_3";
   Data           : constant Multi_Output_Data_Record :=
                      Classifier_Utilities.Load_Data ("src/diabetes.csv");
   X              : constant Value_Data_Lists_2D := Data.Feature_Values;
   Num_Samples    : constant Natural := Natural (X.Length);
   aClassifier    : Base_Decision_Tree.Classifier
     (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   --  Iris_Target (Y) : num outputs x num samples
   Target         : Value_Data_Lists_2D;
   No_Weights     : Weights.Weight_List :=
                      Classifier_Types.Float_Package.Empty_Vector;
   Exporter       : Graphviz_Exporter.DOT_Tree_Exporter;
begin
   Put_Line ("Lesson 3");
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   --  Iris_Target is 2D list num outputs x num samples
   Target := Data.Label_Values;
   Put_Line ("Feature Names:");
   --  Fit function adjusts weights according to data values so that
   --  better accuracy can be achieved
   C_Init (aClassifier, "2", Criterion.Gini_Criteria);
   Classification_Fit (aClassifier, X, Target, No_Weights);
   Printing.Print_Tree ("Diabetes Tree", aClassifier);
   Put_Line ("----------------------------------------------");
   New_Line;

   Graphviz_Exporter.C_Init
     (Exporter, aClassifier.Attributes.Decision_Tree);
   Graphviz_Exporter.Export_Graphviz
     (Exporter, aClassifier.Attributes.Decision_Tree,
      Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_3;
