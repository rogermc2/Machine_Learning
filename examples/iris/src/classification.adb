
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with ML_Types;
with Printing;
with Tree;
with Weights;

package body Classification is
   use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Classify_Iris  is
      use Ada.Containers;
      use Ada.Strings.Unbounded;
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Printing;
--        use ML_Types.String_Package;
      use Classifier_Types.Float_Package;
      Routine_Name    : constant String :=
                          "Classification.Classify_Iris ";
      Iris_Data       : constant Data_Record := Load_Data ("src/iris.csv");
--        Feature_Names   : constant String_List := Iris_Data.Feature_Names;
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      Exporter        : Graphviz_Exporter.DOT_Tree_Exporter;
--        Class_Names     : Class_Names_List;
--        Names_Cursor    : String_Package.Cursor := Feature_Names.First;
--        Features        : Feature_Names_List;
      X               : constant Value_Data_Lists_2D :=
                          Iris_Data.Feature_Values;
      Num_Samples     : constant Natural := Natural (X.Length);
      --  Y: num outputs x num classes
      Y               : Value_Data_Lists_2D;
      No_Weights      : Weights.Weight_List := Empty_Vector;
   begin
      Put_Line (Routine_Name);
--        Class_Names.Append (To_Unbounded_String ("Setosa"));
--        Class_Names.Append (To_Unbounded_String ("Versicolour"));
--        Class_Names.Append (To_Unbounded_String ("Virginica"));
--
--        while Has_Element (Names_Cursor) loop
--           Features.Append (Element (Names_Cursor));
--           Next (Names_Cursor);
--        end loop;
--        Printing.Print_Unbounded_List (Routine_Name & "Features", Features);

      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Iris_Data.Label_Values);
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");

      Classification_Fit (theClassifier, X, Y, No_Weights);
      Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      C_Init (theClassifier, Criterion.Gini_Criteria);
      Classification_Fit (theClassifier, X, Y, No_Weights);
      Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);

      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
--           Class_Names => Class_Names,
--           Feature_Names => Features,
         Output_File_Name => To_Unbounded_String ("iris.dot"));

   end Classify_Iris;

   --  -------------------------------------------------------------------------

end Classification;
