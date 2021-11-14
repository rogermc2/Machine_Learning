--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Criterion;
with Estimator;
with Node_Splitter;
with Printing;
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
        use Weights;
        Num_Outputs     : constant Positive :=
                            Positive (Self.Attributes.Num_Outputs);
        --  L978
        --  Proba: num nodes x num outputs x num classes
        Proba           : constant Weight_Lists_3D :=
                            Tree.Predict (Self.Attributes.Decision_Tree, X);
        Prob_Node       : Weight_Lists_2D;
        Prob_Classes    : Weight_List;
        Prob_Node_Class : Weight_Lists_2D;
        Prob_Outputs    : Weight_Lists_3D;
--          Num_Nodes       : constant Positive := Positive (Proba.Length);
        --  Classes: num outputs x num classes
--          Classes         : constant Value_Data_Lists_2D :=
--                              Self.Attributes.Decision_Tree.Classes;
--          Num_Classes     : constant Positive := Positive (Classes.Length);
        Output_K        : Weight_Lists_2D; -- List of nodes
        Node_K          : Weight_List;     -- List of classes
        All_Proba       : Value_Data_Lists_2D;
        F_Class         : Float;
        Normalizer      : Float;
    begin
        Put_Line ("Decision_Tree_Classification.Predict_Probability Num_Outputs length: "
                 & Integer'Image (Num_Outputs));
        Put_Line ("Decision_Tree_Classification.Predict_Probability Proba length: "
                 & Integer'Image (Integer (Proba.Length)));
        Printing.Print_Weight_Lists_3D
          ("Decision_Tree_Classification.Predict_Probability Proba", Proba);
        for index in 1 .. Num_Outputs loop
            Prob_Node_Class.Clear;
            for index_2 in Proba.First_Index .. Proba.Last_Index loop
                --  Prob_Node: num outputs x num classes
                Prob_Node := Proba.Element (index_2);
                for index_3 in Prob_Node.First_Index .. Prob_Node.Last_Index loop
                    --  Prob_Classes: num classes
                    Prob_Classes := Prob_Node.Element (index_3);
                end loop;
                 Prob_Node_Class.Append (Prob_Classes);
            end loop;
            Prob_Outputs.Append (Prob_Node_Class);
        end loop;
        Put_Line ("Decision_Tree_Classification.Predict_Probability Prob_Outputs length: "
                 & Integer'Image (Integer (Prob_Outputs.Length)));
        Printing.Print_Weight_Lists_3D
          ("Decision_Tree_Classification.Predict_Probability Prob_Outputs",
          Prob_Outputs);

        --  L969
        for k in Prob_Outputs.First_Index .. Prob_Outputs.Last_Index loop
            Output_K := Prob_Outputs.Element (k);
            Printing.Print_Weights_Lists_2D
              ("Decision_Tree_Classification.Predict_Probability Output_K",
               Output_K);
            for node_index in Output_K.First_Index .. Output_K.Last_Index loop
                Prob_Classes := Output_K.Element (node_index);
                Normalizer := 0.0;
                for class_index in Node_K.First_Index  .. Node_K.Last_Index loop
                    F_Class := Node_K.Element (class_index);
                    for class_index in Prob_Classes.First_Index ..
                      Prob_Classes.Last_Index loop
                        F_Class := Prob_Classes.Element (class_index);
                        Normalizer := Normalizer + F_Class;
                    end loop;

                    if Normalizer > 0.0 then
                        for class_index in Prob_Classes.First_Index ..
                          Prob_Classes.Last_Index loop
                            F_Class := Prob_Classes.Element (class_index);
                            Prob_Classes.Replace_Element
                              (class_index, F_Class / Normalizer);
                        end loop;
                    end if;
                end loop;

                Output_K.Replace_Element (node_index, Prob_Classes);
            end loop;
            Prob_Outputs.Replace_Element (k, Output_K);
        end loop;

        return All_Proba;

    end Predict_Probability;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classification;
