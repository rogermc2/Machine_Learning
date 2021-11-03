--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Criterion;
with Estimator;
with Node_Splitter;
--  with Printing;
with Weights;

package body Decision_Tree_Classification is

   --  -------------------------------------------------------------------------
   --  L884
   procedure Classification_Fit
     (aClassifier    : in out Base_Decision_Tree.Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y              : ML_Types.Value_Data_Lists_2D;
      Max_Depth      : Integer := -1) is
      use Estimator;
      Sample_Weights : Classifier_Types.Float_List;
   begin
      Assert (aClassifier.Estimator_Kind = Classifier_Estimator,
              "Decision_Tree_Classifer.Classification_Fit, invalid estimator "
              & Estimator_Type'Image (aClassifier.Estimator_Kind));

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
                                 return Weights.Weight_Lists_3D is
      use ML_Types;
      use Weights;
      Num_Outputs     : constant Positive := Positive (X.Element (1).Length);
      --  L954
      Proba           : constant Weights.Weight_Lists_3D :=
                          Self.Attributes.Decision_Tree.Values;
      Num_Nodes       : constant Positive := Positive (Proba.Length);
      Classes         : constant Value_Data_Lists_2D :=
                          Self.Attributes.Decision_Tree.Classes;
      Num_Classes     : constant Positive := Positive (Classes.Length);
      Prob_K          : Weight_Lists_2D;
      Prob_Class      : Weight_List;
      All_Proba       : Weight_Lists_3D;
      Class           : Float;
      Normalizer      : Float;
   begin
      --  L969
      for k in 1 .. Num_Outputs loop
         Prob_K.Clear;
         for node_index in 1 .. Num_Nodes loop
            Prob_K := Proba.Element (node_index);
            Prob_Class.Clear;
            for class_index in 1  .. Num_Classes loop
               Prob_Class := Prob_K.Element (class_index);

               Normalizer := 0.0;
               for index in 1 .. Num_Nodes loop
                  Class := Prob_Class.Element (class_index);
                  Normalizer := Normalizer + Class;
               end loop;

               if Normalizer > 0.0 then
                  for index in Prob_Class.First_Index ..
                    Prob_Class.Last_Index loop
                     Class := Prob_Class.Element (class_index);
                     Class := Class / Normalizer;
                     Prob_Class.Replace_Element  (index, Class);
                  end loop;
               end if;

               Prob_K.Append (Prob_Class);
            end loop;
            All_Proba.Append (Prob_K);
         end loop;
      end loop;

      return All_Proba;

   end Predict_Probability;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classification;
