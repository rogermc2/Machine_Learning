--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Utilities;
with Criterion;
with Estimator;
with Node_Splitter;
--  with Printing;
with Utilities;
with Weights;

package body Decision_Tree_Classification is

    --  -------------------------------------------------------------------------
    --  L904
    procedure Classification_Fit
      (aClassifier    : in out Base_Decision_Tree.Classifier;
       X              : ML_Types.Value_Data_Lists_2D;
       Y              : ML_Types.Value_Data_Lists_2D;
       Sample_Weights : in out Weights.Weight_List) is
        use Estimator;
        Routine_Name : constant String := "Decision_Tree_Classifer.Classification_Fit ";
    begin
        Assert (aClassifier.Estimator_Kind = Classifier_Estimator,
                Routine_Name & " invalid estimator "
                & Estimator_Type'Image (aClassifier.Estimator_Kind));

        --  L920 X is 2D list num samples x num features
        --       Y is 2D list num classes x num outputs
        Base_Decision_Tree.Base_Fit (aClassifier, X, Y, Sample_Weights);

    end Classification_Fit;

    --  -------------------------------------------------------------------------

    --  L852 DecisionTreeClassifier.__init__
    procedure C_Init
      (aClassifier              : in out Base_Decision_Tree.Classifier;
       Min_Split_Samples        : String;
       Criterion_Type           : Criterion.Classifier_Criteria_Type :=
         Criterion.Gini_Criteria;
       Max_Depth                : Integer := -1;
       Min_Leaf_Samples         : Positive := 1;
       Min_Leaf_Weight_Fraction : Float := 0.0;
       Max_Features             : Tree.Index_Range := Tree.Index_Range'Last;
       Max_Leaf_Nodes           : Integer := -1;
       Class_Weight             : Weights.Weight_Type := Weights.No_Weight;
       Min_Impurity_Decrease    : Float := 0.0;
       CCP_Alpha                : Float := 0.0;
       Random_State             : Integer := 0) is
       use Base_Decision_Tree;
    begin
        if Utilities.Is_Float (To_Unbounded_String (Min_Split_Samples)) then
            declare
                Min_Split : Split_Value_Record (Split_Float);
            begin
                Min_Split.Float_Value := Float'Value (Min_Split_Samples);
                aClassifier.Parameters.Min_Samples_Split := Min_Split;
            end;
        elsif Utilities.Is_Integer
          (To_Unbounded_String (Min_Split_Samples)) then
            declare
                Min_Split : Base_Decision_Tree.Split_Value_Record
                  (Split_Integer);
            begin
                Min_Split.Integer_Value := Integer'Value (Min_Split_Samples);
                aClassifier.Parameters.Min_Samples_Split := Min_Split;
            end;

        end if;
        aClassifier.Parameters.Max_Depth := Max_Depth;
        aClassifier.Parameters.Min_Samples_Leaf := Min_Leaf_Samples;
        aClassifier.Parameters.Min_Weight_Fraction_Leaf := Min_Leaf_Weight_Fraction;
        aClassifier.Parameters.Max_Features := Max_Features;
        aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
        aClassifier.Parameters.Min_Impurity_Decrease := Min_Impurity_Decrease;
        aClassifier.Parameters.Class_Weight := Class_Weight;
        aClassifier.Parameters.CCP_Alpha := CCP_Alpha;
        aClassifier.Parameters.Random_State := Random_State;

        aClassifier.Parameters.Criterion_Kind := Criterion_Type;
        Node_Splitter.C_Init (aClassifier.Parameters.Splitter);

    end C_Init;

    --  -------------------------------------------------------------------------
    --  _classes.py L952 Predict_Probability predicts class probabilities
    --   of the input samples X.
    --  The predicted class probability is the fraction of samples of the same
    --  class in a leaf.
    function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                  X    : ML_Types.Value_Data_Lists_2D)
                                 return Weights.Weight_Lists_3D is
        use Weights;
        Num_Outputs : constant Positive :=
                        Positive (Self.Attributes.Num_Outputs);
        --  L978
        --  Proba: num samples x num outputs x num classes
        Proba       : constant Weight_Lists_3D :=
                        Tree.Predict (Self.Attributes.Decision_Tree, X);
        Output_K    : Weight_Lists_2D;
        Node_K      : Weight_List;
        --  All_Proba: num samples x num outputs x num classes
        All_Proba   : Weight_Lists_3D;
        F_Class     : Float;
        Normalizer  : Float;
    begin
        --  All_Proba: num_outputs x num samples x num classes
        All_Proba := Classifier_Utilities.Samples_3D_To_Outputs_3D
          (Proba, Num_Outputs);
        --  L969
        for output_index in All_Proba.First_Index .. All_Proba.Last_Index loop
            Output_K := All_Proba.Element (output_index);
            --  Output_K: list of num nodes x num classes
            for node_index in Output_K.First_Index .. Output_K.Last_Index loop
                Node_K := Output_K.Element (node_index);
                --  Node_K List of classes
                for class_index in Node_K.First_Index  .. Node_K.Last_Index loop
                    Normalizer := 0.0;
                    F_Class := Node_K.Element (class_index);
                    for class_index in Node_K.First_Index ..
                      Node_K.Last_Index loop
                        F_Class := Node_K.Element (class_index);
                        Normalizer := Normalizer + F_Class;
                    end loop;

                    if Normalizer > 0.0 then
                        for class_index in Node_K.First_Index ..
                          Node_K.Last_Index loop
                            F_Class := Node_K.Element (class_index);
                            Node_K.Replace_Element
                              (class_index, F_Class / Normalizer);
                        end loop;
                    end if;
                end loop;

                Output_K.Replace_Element (node_index, Node_K);
            end loop;

            All_Proba.Replace_Element (output_index, Output_K);
        end loop;

        return All_Proba;

    end Predict_Probability;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classification;
