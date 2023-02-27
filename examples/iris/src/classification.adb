
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Loader;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with ML_Types;
with NL_Types;
with Tree;
with Tree_Printing;
with Weights;

package body Classification is
   use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Classify_Iris  is
      use Ada.Containers;
      use Ada.Strings.Unbounded;
      use Classifier_Loader;
      use Decision_Tree_Classification;
      use NL_Types.Float_Package;
      use Tree_Printing;
      Routine_Name    : constant String := "Classification.Classify_Iris ";
      Iris_Data       : constant Multi_Output_Data_Record :=
                            Load_Data ("src/iris.csv");
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      Exporter        : Graphviz_Exporter.DOT_Tree_Exporter;
      Class_Names     : Class_Names_List;
      Features        : Feature_Names_List;
      Iris_Features   : constant Value_Data_Lists_2D :=
                          Iris_Data.Feature_Values;
      Num_Samples     : constant Natural := Natural (Iris_Features.Length);
      --  Iris_Target: num outputs x num classes
      Iris_Target     : Value_Data_Lists_2D;
      No_Weights      : Weights.Weight_List := Empty_Vector;
   begin
      Put_Line (Routine_Name);
      Class_Names.Append (To_Unbounded_String ("Setosa"));
      Class_Names.Append (To_Unbounded_String ("Versicolour"));
      Class_Names.Append (To_Unbounded_String ("Virginica"));

      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");

      --  Iris_Target is 2D list num outputs x num classes
      Iris_Target := Iris_Data.Label_Values;
      Assert (Integer (Iris_Target.Length) = Num_Samples, Routine_Name &
                " invalid Iris_Target vector");

      C_Init (theClassifier, "2", Criterion.Gini_Criteria, Max_Features => 2);
      Classification_Fit (theClassifier, Iris_Features, Iris_Target,
                          No_Weights);
      Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      C_Init (theClassifier, "2", Criterion.Gini_Criteria);
      Classification_Fit (theClassifier, Iris_Features, Iris_Target,
                          No_Weights);
      Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);

      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Class_Names => Class_Names,
         Feature_Names => Features,
         Output_File_Name => To_Unbounded_String ("iris.dot"));

   end Classify_Iris;

   --  -------------------------------------------------------------------------

end Classification;
