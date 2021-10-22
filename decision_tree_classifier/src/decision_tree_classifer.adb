--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
--  with Classifier_Utilities;
with Criterion;
with Node_Splitter;
with Weights;

package body Decision_Tree_Classifer is

   --  -------------------------------------------------------------------------
   --  L884
   procedure Classification_Fit
     (aClassifier    : in out Base_Decision_Tree.Classifier;
      X              : ML_Types.List_Of_Value_Data_Lists;
      Y              : ML_Types.List_Of_Value_Data_Lists;
      Max_Depth      : Integer := -1) is
      Sample_Weights : Classifier_Types.Float_List;
   begin
      --  L920
      Base_Decision_Tree.Base_Fit (aClassifier, X, Y, Sample_Weights, Max_Depth);

   end Classification_Fit;

   --  -------------------------------------------------------------------------

   --  L852 DecisionTreeClassifier.__init__
   procedure C_Init (aClassifier              : in out Base_Decision_Tree.Classifier;
                     Criteria                 : Criterion.Criterion_Class;
                     Splitter                 : Node_Splitter.Splitter_Class;
                     Max_Depth                : Integer := -1;
                     Min_Split_Samples        : Positive := 2;
                     Min_Leaf_Samples         : Positive := 1;
                     Min_Leaf_Weight_Fraction : Float := 0.0;
                     Max_Features             : Tree.Index_Range :=
                       Tree.Index_Range'Last;
                     Max_Leaf_Nodes           : Integer := -1;
                     Class_Weight             : Weights.Weight_Type :=
                       Weights.No_Weight;
                     Min_Impurity_Decrease    : Float := 0.0;
                     CCP_Alpha                : Float := 0.0;
                     Random_State             : Integer := 0) is
   begin
      aClassifier.Parameters.Critera := Criteria;
      aClassifier.Parameters.Splitter := Splitter;
      aClassifier.Parameters.Max_Depth := Max_Depth;
      aClassifier.Parameters.Min_Samples_Split := Min_Split_Samples;
      aClassifier.Parameters.Min_Samples_Leaf := Min_Leaf_Samples;
      aClassifier.Parameters.Min_Weight_Fraction_Leaf := Min_Leaf_Weight_Fraction;
      aClassifier.Parameters.Max_Features := Max_Features;
      aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
      aClassifier.Parameters.Min_Impurity_Decrease := Min_Impurity_Decrease;
      aClassifier.Parameters.Class_Weight := Class_Weight;
      aClassifier.Parameters.CCP_Alpha := CCP_Alpha;
      aClassifier.Parameters.Random_State := Random_State;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  _classes.py L930 Predict_Probability predicts class probabilities
   --   of the input samples X.
   --  The predicted class probability is the fraction of samples of the same
   --  class in a leaf.
   function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                 X    : ML_Types.List_Of_Value_Data_Lists)
                                 return ML_Types.List_Of_Value_Data_Lists is
      use ML_Types;
      Num_Outputs     : constant Positive := Positive (X.Element (1).Length);
      Num_Nodes       : constant Positive
        := Positive (Self.Attributes.Decision_Tree.Nodes.Node_Count);
      Num_Classes     : constant Positive
        := Positive (Self.Attributes.Decision_Tree.Classes.Length);
      Proba           : Tree.Values_Array_3D
        (1 .. Num_Nodes, 1 .. Num_Outputs,1 .. Num_Classes);
      Prob_K          : Tree.Values_Array_2D (1 .. Num_Nodes, 1 .. Num_Classes);
      All_Proba       : List_Of_Value_Data_Lists;
      Normalizer      : Float := 0.0;
   begin
      --  L954
      Proba :=  Tree.Predict (Self.Attributes.Decision_Tree, X);
      --        Classifier_Utilities.Print_Value_Data_List ("Decision_Tree_Classifer.Predict_Probability, Proba",
      --                                                    Proba);
      --  L969
      for k in 1 .. Num_Outputs loop
         for node_index in 1 .. Num_Nodes loop
            for class_index in Proba'First (1)  .. Proba'Last (1) loop
               Prob_K (node_index, class_index) :=
                 Proba (node_index, k, class_index);
               if Normalizer <= 0.0 then
                  Normalizer := 1.0;
               end if;
            end loop;
         end loop;
      end loop;

      return All_Proba;

   end Predict_Probability;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
