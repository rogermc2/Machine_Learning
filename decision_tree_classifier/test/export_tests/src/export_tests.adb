
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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

package body Export_Tests is
   use Classifier_Types;
   use ML_Types;
   X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                   ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
   --  Y_Array 6 rows (samples) x 2 columns (features)
   Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
   Y2_Array    : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                   ((-1, 1), (-1, 1), (-1, 1), (1, 2), (1, 2), (1, 3));
   W_Array     : constant Float_Array (1 .. 6) :=
                   (1.0, 1.0, 1.0, 0.5, 0.5, 0.5);
   --     Y_Deg_Array : constant Integer_Array (1 .. 6) := (1, 1, 1, 1, 1, 1);

   --  -------------------------------------------------------------------------

   procedure Test_Graphviz_Toy  is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Float_Package;
      Routine_Name      : constant String :=
                           "Export_Tests.Test_Graphviz_Toy";
      theClassifier     : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
      Class_Names       : Class_Names_List;
      Feature_Names     : Feature_Names_List := Unbounded_Package.Empty_Vector;
      X                 : constant Value_Data_Lists_2D :=
                           To_Multi_Value_List (X_Array);
      Y                 : constant Value_Data_Lists_2D :=
                           To_Integer_Value_List_2D (Y_Array);
      --  Y2 is 2D list num outputs x num classes
      Y2                : constant Value_Data_Lists_2D :=
                           To_Multi_Value_List (Y2_Array);
      W                 : Weights.Weight_List := To_Float_List (W_Array);
      --        Y_Degraded       : constant Value_Data_Lists_2D :=
      --                                To_Integer_Value_List_2D  (Y_Deg_Array);
      Num_Samples       : constant Natural := Natural (X.Length);
      No_Weights        : Weights.Weight_List := Empty_Vector;
   begin
      Class_Names.Append (To_Unbounded_String ("Yes"));
      Class_Names.Append (To_Unbounded_String ("No"));
      Feature_Names.Append (To_Unbounded_String ("feature_1"));
      Feature_Names.Append (To_Unbounded_String ("feature_2"));

      C_Init (theClassifier, "2", Criterion.Gini_Criteria,
               Max_Depth => 3);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0,
              Routine_Name & " called with empty X vector.");

      Classification_Fit (theClassifier, X, Y, No_Weights);
      Printing.Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);

      --  Test with feature_names
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Feature_Names => Feature_Names,
         Output_File_Name => To_Unbounded_String ("features.dot"));

      --  Test with class names
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Class_Names => Class_Names,
         Output_File_Name => To_Unbounded_String ("classes.dot"));

      --  Test plot options
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree, Filled => True,
         Impurity => False, Proportion => True, Special_Characters => True,
         Rounded => True, Font_Name => To_Unbounded_String ("sans"),
         Output_File_Name => To_Unbounded_String ("plot.dot"));

      --  Test max depth
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree, Max_Depth => 0,
         Class_Names => Class_Names,
         Output_File_Name => To_Unbounded_String ("max_depth.dot"));

      --  Test max depth with plot options
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree, Max_Depth => 0,
         Filled => True, Node_Ids => True,
         Output_File_Name => To_Unbounded_String ("max_depth_plot.dot"));

      C_Init (theClassifier, "2", Criterion.Gini_Criteria, Max_Depth => 3);
      Classification_Fit (theClassifier, X, Y2, W);
      Printing.Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);

      --  Test multi-output with weighted samples
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree, Filled => True,
         Impurity => False, Max_Depth => 5,
         Output_File_Name => To_Unbounded_String ("multi_output.dot"));

   end Test_Graphviz_Toy;

   --  -------------------------------------------------------------------------

end Export_Tests;
