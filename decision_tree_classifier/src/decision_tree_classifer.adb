--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
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
       Y_Encoded      : out Classifier_Types.List_Of_Natural_Lists;
       Classes        : out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weights : out Classifier_Types.Float_List) is
    begin
        --  L929
        Base_Decision_Tree.Base_Fit (aClassifier, X, Y, Y_Encoded, Classes,
                                     Sample_Weights);

    end Classification_Fit;

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

    procedure Init (aClassifier       : in out Base_Decision_Tree.Classifier;
                    Input_X           : ML_Types.List_Of_Value_Data_Lists;
                    Max_Leaf_Nodes    : Integer := -1;
                    Max_Depth         : Integer := -1;
                    Min_Samples_Split : Positive := 2;
                    Min_Leaves        : Positive := 1;
                    Random_State      : Integer := 0) is
        Num_Outputs     : constant Tree.Index_Range :=
                            Tree.Index_Range (Input_X.Element (1).Length);
        Min_Leaf_Weight : constant Float := 0.0;
        Max_Features    : constant Tree.Index_Range := 1;
        Criteria        : Criterion.Criterion_Class;
        Splitter        : Node_Splitter.Splitter_Class;
        Y_Encoded       : Classifier_Types.List_Of_Natural_Lists;
        Sample_Weight   : Weights.Weight_List;
    begin
        Criterion.C_Init (Criteria, Num_Outputs, aClassifier.Attributes.Classes);
        Node_Splitter.C_Init (Splitter, Criteria,
                              aClassifier.Parameters.Max_Features,
                              aClassifier.Parameters.Min_Samples_Leaf,
                              Min_Leaf_Weight);
        Node_Splitter.Init (Splitter, Input_X, Y_Encoded, Sample_Weight);
        Base_Decision_Tree.C_Init (aClassifier       => aClassifier,
                                   Criteria          => Criteria,
                                   Splitter          => Splitter,
                                   Min_Samples_Split => Min_Samples_Split,
                                   Min_Leaf_Samples  => Min_Leaves,
                                   Max_Features      => Max_Features,
                                   Max_Depth         => Max_Depth,
                                   Max_Leaf_Nodes    => Max_Leaf_Nodes,
                                   Random_State       => Random_State);
    end Init;

    --  -------------------------------------------------------------------------
    --  L930 Predict_Probability predicts class probabilities of the
    --  input samples X.
    --  The predicted class probability is the fraction of samples of the same
    --  class in a leaf.
    function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                  X    : ML_Types.List_Of_Value_Data_Lists)
                                  return ML_Types.List_Of_Value_Data_Lists is
        use ML_Types;
        Proba      : Value_Data_List;
        All_Proba  : List_Of_Value_Data_Lists;
        Data       : Value_Record;
        Normalizer : Float := 0.0;
    begin
        --  L954
        Proba :=  Tree.Predict (Self.Attributes.Decision_Tree, X);
        for OP_Index in 1 .. Self.Attributes.Num_Outputs loop
            for index in Proba.First_Index .. Proba.Last_Index loop
                Data := Proba.Element (index);
                case Data.Value_Kind is
                when Float_Type =>
                    Normalizer := Normalizer + Data.Float_Value;
                when Integer_Type =>
                    Normalizer := Normalizer + Float (Data.Integer_Value);
                when others => null;
                end case;
            end loop;
            if Normalizer <= 0.0 then
                Normalizer := 1.0;
            end if;

            for index in Proba.First_Index .. Proba.Last_Index loop
                Data := Proba.Element (index);
                case Data.Value_Kind is
                when Float_Type =>
                    Data.Float_Value := Data.Float_Value / Normalizer;
                when Integer_Type =>
                    Data.Integer_Value :=
                      Integer (Float (Data.Integer_Value) / Normalizer);
                when others => null;
                end case;
                Proba.Replace_Element (index, Data);
            end loop;
            All_Proba.Append (Proba);

        end loop;

        return All_Proba;

    end Predict_Probability;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
