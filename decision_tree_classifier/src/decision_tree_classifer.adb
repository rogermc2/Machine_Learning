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
      X              : ML_Types.Value_Data_Lists_2D;
      Y              : ML_Types.Value_Data_Lists_2D;
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
                                 X    : ML_Types.Value_Data_Lists_2D)
                                  return Tree.Values_List_2D is
      use ML_Types;
      Num_Outputs     : constant Positive := Positive (X.Element (1).Length);
      Num_Nodes       : constant Positive
        := Positive (Self.Attributes.Decision_Tree.Nodes.Node_Count);
      Classes         : constant Value_Data_Lists_2D :=
                          Self.Attributes.Decision_Tree.Classes;
      Num_Classes     : constant Positive := Positive (Classes.Length);
      Proba           : Tree.Values_List_3D;
      Classes_K       : Value_Data_List;
      Num_Classes_K   : Positive;
      Prob_K          : Tree.Values_List_2D;
      Prob_K_List     : Tree.Values_List;
      All_Proba       : Tree.Values_List_2D;
      Normalizer      : Float;
   begin
      --  L954
      Proba :=  Tree.Predict (Self.Attributes.Decision_Tree, X);
      --        Classifier_Utilities.Print_Value_Data_List ("Decision_Tree_Classifer.Predict_Probability, Proba",
      --                                                    Proba);
      --  L969
      for k in 1 .. Num_Outputs loop
         Prob_K_List.Clear;
         for node_index in 1 .. Num_Nodes loop
            for class_index in 1  .. Num_Classes loop
               Classes_K := Classes.Element (k);
               Num_Classes_K := Positive (Classes_K.Length);
               Prob_K := Proba.Element (node_index);
               --                      Prob_K (node_index, class_index) := Proba (node_index, k,
               --                                                                 Num_Classes_K);
               Normalizer := 0.0;
               for index in 1 .. Num_Nodes loop
                  Normalizer := Normalizer + Prob_K (index, class_index);
               end loop;

               Prob_K_List.Append (Prob_K (node_index, class_index));

               if Normalizer > 0.0 then
                  for index in Prob_K_List.First_Index ..
                    Prob_K_List.Last_Index loop
                     Prob_K_List.Replace_Element
                       (index, Prob_K_List.Element (index) / Normalizer);
                  end loop;
               end if;
            end loop;
         end loop;

         All_Proba.Append (Prob_K_List);
      end loop;

      return All_Proba;

   end Predict_Probability;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
