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

      --  L920 X is 2D list num samples x num features
      --       Y is 2D list num classes x num outputs
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
   --  _classes.py L952 Predict_Probability predicts class probabilities
   --   of the input samples X.
   --  The predicted class probability is the fraction of samples of the same
   --  class in a leaf.
   function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                 X    : ML_Types.Value_Data_Lists_2D)
                                 return ML_Types.Value_Data_Lists_2D is
      use ML_Types;
      Num_Outputs     : constant Positive := Positive (X.Element (1).Length);
      --  L978
      Proba           : constant Value_Data_Lists_2D :=
                          Base_Decision_Tree.Predict (Self, X);
      Num_Nodes       : constant Positive := Positive (Proba.Length);
      Classes         : constant Value_Data_Lists_2D :=
                          Self.Attributes.Decision_Tree.Classes;
      Num_Classes     : constant Positive := Positive (Classes.Length);
      Prob_K          : Value_Data_List;
      Prob_Class      : Value_Record;
      All_Proba       : Value_Data_Lists_2D;
      F_Class         : Float;
      I_Class         : Integer;
      Normalizer      : Float;
   begin
      --  L969
      for k in 1 .. Num_Outputs loop
         Prob_K.Clear;
         for node_index in 1 .. Num_Nodes loop
            Prob_K := Proba.Element (node_index);
            for class_index in 1  .. Num_Classes loop
               Prob_Class := Prob_K.Element (class_index);
               Normalizer := 0.0;
               case Prob_Class.Value_Kind is
                  when Float_Type =>
                     F_Class := Prob_Class.Float_Value;
                     Normalizer := Normalizer + F_Class;
                  when Integer_Type =>
                     I_Class := Prob_Class.Integer_Value;
                     Normalizer := Normalizer + Float (I_Class);
                  when others => null;
               end case;
            end loop;

            if Normalizer > 0.0 then
               case Prob_Class.Value_Kind is
                  when Float_Type =>
                     Prob_Class.Float_Value :=
                       Prob_Class.Float_Value / Normalizer;
                  when Integer_Type =>
                     Prob_Class.Integer_Value :=
                       Integer (Float (Prob_Class.Integer_Value) / Normalizer);
                  when others => null;
               end case;
            end if;

            Prob_K.Append (Prob_Class);
         end loop;
         All_Proba.Append (Prob_K);
      end loop;

      return All_Proba;

   end Predict_Probability;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classification;
