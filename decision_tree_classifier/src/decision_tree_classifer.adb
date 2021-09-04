--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Classifier_Types;
with Encode_Utils;
with Tree_Build;

package body Decision_Tree_Classifer is

    procedure Build_Best_First_Tree
      (X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       aTree : in out Tree.Tree_Data);
    procedure Build_Depth_First_Tree
      (X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       aTree : in out Tree.Tree_Data);

    --  -------------------------------------------------------------------------

    procedure Build_Tree (Self : in out Classifier;
                          X, Y : ML_Types.List_Of_Value_Data_Lists;
                          Sample_Weight : Classifier_Types.Weight_List) is
    --        Criterion : Classifier_Criteria_Type := Self.Parameters.Criterion;
    --        Splitter  : Splitter_Type := Self.Parameters.Splitter;
        theTree   : Tree.Tree_Data
          (Self.Attributes.Num_Features, Self.Attributes.Num_Outputs,
           Tree.Index_Range (Self.Attributes.Classes.Length));
    begin
        --  if is_classifier(self):
        Self.Attributes.Decision_Tree := theTree;

        if Self.Parameters.Max_Leaf_Nodes < 0 then
            Build_Depth_First_Tree (X, Y, Sample_Weight, theTree);
        else
            Build_Best_First_Tree (X, Y, Sample_Weight, theTree);
        end if;
    end Build_Tree;

    --  -------------------------------------------------------------------------

    procedure Build_Best_First_Tree
      (X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       aTree : in out Tree.Tree_Data) is
        use Tree_Build;
        Builder : Tree_Builder (Best_First_Tree);
    begin
        Build (Builder, X, Y, Sample_Weight, aTree);

    end Build_Best_First_Tree;

    --  -------------------------------------------------------------------------

    procedure Build_Depth_First_Tree
      (X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       aTree : in out Tree.Tree_Data) is
        use Tree_Build;
        Builder : Tree_Builder (Depth_First_Tree);
    begin
        Build (Builder, X, Y, Sample_Weight, aTree);
    end Build_Depth_First_Tree;

    --  -------------------------------------------------------------------------

    procedure Check_Parameters is
    begin
        null;
    end Check_Parameters;

    --  -------------------------------------------------------------------------
    --  if is_classification: part of Python BasesDecisionTree.Fit
    procedure Classification_Fit
      (aClassifier   : in out Classifier;
       Y : in out ML_Types.List_Of_Value_Data_Lists; Num_Outputs : Positive;
       Y_Encoded : out ML_Types.List_Of_Value_Data_Lists;
       Expanded_Class_Weight : out Classifier_Types.Float_List) is
        use ML_Types;
        use Weights;
        Y_Original : ML_Types.List_Of_Value_Data_Lists;
        Y_K        : Value_Data_List;
        Classes_K  : Value_Data_List;
        Inverse    : Natural_List;
    begin
        for k in 1 .. Num_Outputs loop
            Y_K := Y.Element (k);
            Classes_K := Encode_Utils.Unique (Y_K, Inverse);
            Y_Encoded.Replace_Element (k, Classes_K);
            aClassifier.Attributes.Classes.Append (Classes_K);
        end loop;
        Y := Y_Encoded;

        if aClassifier.Parameters.Class_Weight /= Weights.No_Weight then
            --  y_original = np.copy(y)
            Expanded_Class_Weight :=
              Weights.Compute_Sample_Weight (Weights.No_Weight, Y_Original);
        end if;

        if aClassifier.Parameters.Class_Weight /= No_Weight then
            Expanded_Class_Weight := Weights.Compute_Sample_Weight
              (aClassifier.Parameters.Class_Weight, Y_Original);
        end if;

    end Classification_Fit;

    --  -------------------------------------------------------------------------
    --  The Fit function adjusts weights according to data values so that
    --  better accuracy can be achieved
    --  Based on tree/_classes.py BaseDecisionTree.Fit
    --  X :  a (n_samples, n_features) matrix of training samples
    --  Y :  a (n_samples, n_outputs) array of integer valued class labels
    --       for the training samples.
    --  Sample_Weight : array-like of shape (n_samples,), default=None
    function Fit (aClassifier   : in out Classifier;
                  X             : ML_Types.List_Of_Value_Data_Lists;
                  Y             : in out ML_Types.List_Of_Value_Data_Lists;
                  Sample_Weight : in out Classifier_Types.Weight_List;
                  Check_Input   : Boolean := False)
                  return Estimator.Estimator_Data is
        use Ada.Containers;
        use ML_Types;
        use Classifier_Types;
        use Classifier_Types.Integer_Package;
        use Value_Data_Package;
        Num_Outputs           : constant Positive := Positive (Y.Length);
        Num_Features          : constant Class_Range :=
                                  Class_Range (X.Length);
        Num_Samples           : constant Positive :=
                                  Positive (X.Element (1).Length);
        --        Random_State          : Integer := aClassifier.Parameters.Random_State;
        Expanded_Class_Weight : Classifier_Types.Float_List;
        theEstimator          : Estimator.Estimator_Data
          (Num_Samples, Positive (Num_Features));
        Y_Encoded             : List_Of_Value_Data_Lists;
        --        Encode_Value          : Value_Record (Float_Type);
        Max_Leaf_Nodes        : Integer := -1;
    begin
        if aClassifier.Parameters.CCP_Alpha < 0.0 then
            raise Value_Error with
              "Decision_Tree_Classifer.Fit CCP_Alpha must be greater than or equal to 0";
        end if;

        if Check_Input then
            null;
        end if;

        aClassifier.Attributes.Num_Features := Tree.Index_Range (Num_Samples);
        aClassifier.Attributes.Num_Outputs := Tree.Index_Range (Num_Outputs);
        aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
        aClassifier.Attributes.Classes.Clear;
        aClassifier.Attributes.Num_Classes.Clear;

        if Positive (Y.Length) /= Num_Samples then
            raise Value_Error with
              "Decision_Tree_Classifer.Fit Number of labels =" &
              Count_Type'Image (Y.Length) & " does not match number of samples ="
              & Integer'Image (Num_Samples);
        end if;

        Classification_Fit (aClassifier, Y, Num_Outputs, Y_Encoded,
                            Expanded_Class_Weight);

        if aClassifier.Parameters.Max_Leaf_Nodes > 0 then
            Max_Leaf_Nodes := aClassifier.Parameters.Max_Leaf_Nodes;
        end if;

        Check_Parameters;

        if not Expanded_Class_Weight.Is_Empty then
            if Sample_Weight.Is_Empty then
                Sample_Weight := Expanded_Class_Weight;
            else
                for index in Sample_Weight.First_Index ..
                  Sample_Weight.Last_Index loop
                    Sample_Weight.Replace_Element
                      (index, Sample_Weight.Element (index) *
                           Expanded_Class_Weight.Element (index)) ;
                end loop;
            end if;
        end if;

        Build_Tree (aClassifier, X, Y, Sample_Weight);

        return theEstimator;
    end Fit;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
