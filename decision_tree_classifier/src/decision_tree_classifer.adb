--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Text_IO; use Ada.Text_IO;

with Ada_Tree_Build;
with Classifier_Types;
with Classifier_Utilities;
with Encode_Utils;
with Tree_Build;

package body Decision_Tree_Classifer is

  procedure Base_Fit_Checks
      (aClassifier   : in out Classifier;
       X             : in out ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : in out Classifier_Types.Float_List);
    procedure Prune_Tree (Self : in out Classifier);

    --  -------------------------------------------------------------------------
    --  Based on class.py fit L350 Build tree
    procedure Build_Tree (Self              : in out Classifier;
                          Min_Samples_Split : Natural;
                          Min_Samples_Leaf  : Natural;
                          Min_Weight_Leaf   : Float;
                          Max_Depth         : Natural;
                          X, Y              : ML_Types.List_Of_Value_Data_Lists;
                          Sample_Weight     : Classifier_Types.Weight_List) is
        use Tree;
        Max_Leaf_Nodes        : constant Integer := Self.Parameters.Max_Leaf_Nodes;
        Splitter              : constant Node_Splitter.Splitter_Class :=
                                  Self.Parameters.Splitter;
        theTree               : Tree.Tree_Class;
        Min_Impurity_Decrease : constant Float := 0.0;
    begin
        --  L387
        --  if is_classifier(self):
        Self.Attributes.Decision_Tree := theTree;

        --  L388
        theTree.Num_Features := Natural (Self.Attributes.Num_Features);
        theTree.Num_Outputs := Self.Attributes.Num_Outputs;
        Self.Attributes.Decision_Tree := theTree;

        --  L398
        if Max_Leaf_Nodes < 0 then
            declare
                Builder : Tree_Build.Tree_Builder (Tree_Build.Depth_First_Tree);
            begin
                --  L419  Depth First case
                Tree_Build.Init_Depth_First_Tree
                  (Builder, Splitter, Min_Samples_Split, Min_Samples_Leaf,
                   Min_Weight_Leaf, Max_Depth, Min_Impurity_Decrease);
                Tree_Build.Build_Depth_First_Tree
                  (Builder, theTree, X, Y, Sample_Weight);
            end;

        else
            declare
                Builder : Tree_Build.Tree_Builder (Tree_Build.Best_First_Tree);
            begin            --  L419  Best First case
                Tree_Build.Init_Best_First_Tree
                  (Builder, Self.Parameters.Splitter, Min_Samples_Split,
                   Min_Samples_Leaf, Min_Weight_Leaf, Max_Depth, Max_Leaf_Nodes,
                   Min_Impurity_Decrease);
                Tree_Build.Build_Best_First_Tree
                  (Builder, theTree, X, Y, Sample_Weight);
            end;
        end if;

        --  L420
        if Self.Attributes.Num_Outputs = 1 then  --  and is_Classifer (Self)
            null;
        end if;

        Prune_Tree (Self);

    end Build_Tree;

    --  -------------------------------------------------------------------------

    procedure Check_Parameters is
    begin
        null;
    end Check_Parameters;

    --  -------------------------------------------------------------------------
    --  if is_classification: part of Python BasesDecisionTree.Fit
    --  L150
    procedure Base_Fit
      (aClassifier   : in out Classifier;
       X             : in out ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : in out Classifier_Types.Float_List) is
        use ML_Types;
        use Weights;
        Num_Samples           : constant Positive :=
                                  Positive (X.Element (1).Length);
        Y_Encoded             : ML_Types.List_Of_Value_Data_Lists;
        --  L205
        Y_Copy                : ML_Types.List_Of_Value_Data_Lists := Y;
        Y_Original            : ML_Types.List_Of_Value_Data_Lists;
        Y_K                   : Value_Data_List;
        Classes_K             : Value_Data_List;
        Inverse               : Natural_List;
        Expanded_Class_Weight : Weight_List;
        Max_Depth             : Natural := Integer'Last;
        --  L226
        Max_Leaf_Nodes        : Integer := aClassifier.Parameters.Max_Leaf_Nodes;
        Min_Sample_Leaf       : Leaf_Record (Tree.Integer_Type);
        Min_Sample_Split      : Split_Record (Tree.Integer_Type);
        Max_Features          : Tree.Features_Record (Tree.Integer_Type);
    begin
        --  L184
        aClassifier.Attributes.Num_Features := Tree.Index_Range (X.Element (1).Length);
        --  L207
        aClassifier.Attributes.Classes.Clear;
        aClassifier.Attributes.Num_Classes.Clear;

        if aClassifier.Parameters.Class_Weight /= No_Weight then
            Y_Original := Y;
        end if;

        for k in Y.First_Index .. Y.Last_Index loop
            Y_K := Y.Element (k);
            Classes_K := Encode_Utils.Unique (Y_K, Inverse);
            Y_Encoded.Append (Classes_K);
            aClassifier.Attributes.Classes.Append (Classes_K);
        end loop;

        --  L218
        Y_Copy := Y_Encoded;
        if aClassifier.Parameters.Class_Weight /= Weights.No_Weight then
            Expanded_Class_Weight :=
              Weights.Compute_Sample_Weight (Weights.No_Weight, Y_Original);
        end if;

        --  L225
        if aClassifier.Parameters.Max_Depth >= 0 then
            Max_Depth := aClassifier.Parameters.Max_Depth;
        end if;

        Base_Fit_Checks (aClassifier, X, Y, Sample_Weight);

    end Base_Fit;

    --  -------------------------------------------------------------------------

  procedure Base_Fit_Checks
      (aClassifier   : in out Classifier;
       X             : in out ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : in out Classifier_Types.Float_List) is
        Num_Samples           : constant Positive :=
                                  Positive (X.Element (1).Length);
        --  L226
        Max_Leaf_Nodes        : Integer := aClassifier.Parameters.Max_Leaf_Nodes;
        Min_Sample_Leaf       : Leaf_Record (Tree.Integer_Type);
        Min_Sample_Split      : Split_Record (Tree.Integer_Type);
        Max_Features          : Tree.Features_Record (Tree.Integer_Type);
    begin
        --  L235
        case aClassifier.Parameters.Min_Samples_Leaf.Leaf_Type is
            when Tree.Integer_Type =>
                if aClassifier.Parameters.Min_Samples_Leaf.Min_Leaf < 1 then
                    raise Value_Error with
                      "Decision_Tree_Classifer.Base_Fit_Checks, Min_Samples_Leaf must be at least 1";
                end if;
                Min_Sample_Leaf.Min_Leaf := aClassifier.Parameters.Min_Samples_Leaf.Min_Leaf;

            when Tree.Float_Type =>
                --  L243
                if aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf <= 0.0 or
                  aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf > 0.5 then
                    raise Value_Error with
                      "Decision_Tree_Classifer.Base_Fit_Checks, Min_Samples_Leaf must be in (0.0, 0.5]";
                end if;
                Min_Sample_Leaf.Min_Leaf :=
                  Integer (Float'Ceiling
                           (aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf));
            when others =>
                raise Value_Error with
                  "Decision_Tree_Classifer.Base_Fit_Checks, invalid Min_Samples_Leaf Leaf_Type";
        end case;

        --  L250
        case aClassifier.Parameters.Min_Samples_Split.Split_Type is
            when Tree.Integer_Type =>
                if aClassifier.Parameters.Min_Samples_Split.Min_Split < 2 then
                    raise Value_Error with
                      "Decision_Tree_Classifer.Base_Fit_Checks, Min_Samples_Split must be at least 2";
                end if;
                Min_Sample_Split.Min_Split := aClassifier.Parameters.Min_Samples_Split.Min_Split;
            when Tree.Float_Type =>
                --  L253
                if aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split <= 0.0 or
                  aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split > 1.0 then
                    raise Value_Error with
                      "Decision_Tree_Classifer.Base_Fit_Checks, Min_Samples_Split must be in (0.0, 1.0]";
                end if;
                --  260
                Min_Sample_Split.Min_Split :=
                  Integer (Float'Ceiling
                           (aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split));
                if Min_Sample_Split.Min_Split < 2 then
                    Min_Sample_Split.Min_Split := 2;
                end if;
            when others =>
                raise Value_Error with
                  "Decision_Tree_Classifer.Base_Fit_Checks, invalid Min_Samples_Split Split_Type";
        end case;

        --  L263
        if Min_Sample_Split.Min_Split < 2 * Min_Sample_Leaf.Min_Leaf then
            Min_Sample_Split.Min_Split := 2 * Min_Sample_Leaf.Min_Leaf;
        end if;

        case aClassifier.Parameters.Max_Features.Feature_Kind is
            when Tree.Enum_Type => null;
            when Tree.Float_Type =>
                if aClassifier.Parameters.Max_Features.Fraction_Features > 0.0 then
                    Max_Features.Max_Features := Integer (aClassifier.Parameters.Max_Features.Fraction_Features) *
                      Integer (aClassifier.Attributes.Num_Features);
                    if Max_Features.Max_Features < 1 then
                        Max_Features.Max_Features := 1;
                    end if;
                end if;
            when Tree.Integer_Type =>
                Max_Features.Max_Features := aClassifier.Parameters.Max_Features.Max_Features;
        end case;

        --  L291
        aClassifier.Parameters.Max_Features := Max_Features;

        if Positive (Y.Element (1).Length) /= Num_Samples then
            raise Value_Error with
              "Decision_Tree_Classifer.Base_Fit_Checks, number of labels " &
              Integer'Image (Integer (Y.Element (1).Length)) &
              " does not match number of samples " & Integer'Image (Num_Samples);
        end if;

        if aClassifier.Parameters.Min_Weight_Fraction_Leaf <= 0.0 or
          aClassifier.Parameters.Min_Weight_Fraction_Leaf > 5.0 then
            raise Value_Error with
              "Decision_Tree_Classifer.v, Min_Weight_Fraction_Leaf must be in (0.0, 5.0]";
        end if;

    end Base_Fit_Checks;

    --  -------------------------------------------------------------------------

    procedure Classification_Fit
      (aClassifier   : in out Classifier;
       X             : in out ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : out Classifier_Types.Float_List) is
    begin
        --  L929
        Base_Fit  (aClassifier, X, Y, Sample_Weight);
    end Classification_Fit;

    --  -------------------------------------------------------------------------
    --  The Fit function adjusts weights according to data values so that
    --  better accuracy can be achieved
    --  Based on tree/_classes.py BaseDecisionTree.Fit
    --  X :  a (n_samples, n_features) matrix of training samples
    --  Y :  a (n_samples, n_outputs) array of integer valued class labels
    --       for the training samples.
    --  Sample_Weight : array-like of shape (n_samples,), default=None
    procedure Fit (aClassifier   : in out Classifier;
                   X             : ML_Types.List_Of_Value_Data_Lists;
                   Y             : in out ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight : in out Classifier_Types.Weight_List;
                   Check_Input   : Boolean := False) is
        use Ada.Containers;
        use ML_Types;
        use Classifier_Types;
        use Classifier_Types.Integer_Package;
        theTree               : Tree.Tree_Class;
        Num_Samples           : constant Positive :=  Positive (X (1).Length);
        Num_Outputs           : constant Positive := Positive (Y.Length);
        Num_Labels            : constant Positive := Positive (Y.Element (1).Length);
        --        Random_State          : Integer := aClassifier.Parameters.Random_State;
        Expanded_Class_Weight : Classifier_Types.Float_List;
        Y_Encoded             : List_Of_Value_Data_Lists;
        Max_Leaf_Nodes        : integer := -1;
        --        Min_Samples_Split     : Natural := 0;
        --        Min_Samples_Leaf      : Natural := 0;
        --        Min_Weight_Leaf       : Float := 0.0;
        --        Max_Depth             : Natural := 0;
    begin
        --  L154
        if aClassifier.Parameters.CCP_Alpha < 0.0 then
            raise Value_Error with
              "Decision_Tree_Classifer.Fit CCP_Alpha must be greater than or equal to 0";
        end if;

        --  L156
        if Check_Input then
            null;
        end if;

        --  L184
        aClassifier.Attributes.Num_Features := Tree.Index_Range (Num_Samples);
        aClassifier.Attributes.Num_Outputs := Tree.Index_Range (Num_Outputs);
        aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
        --  L201
        aClassifier.Attributes.Classes.Clear;
        aClassifier.Attributes.Num_Classes.Clear;
        Put_Line ("Decision_Tree_Classifer.Fit Y length: " &
                    Integer'Image (Integer (Y.Length)));
        Put_Line ("Decision_Tree_Classifer.Fit Y.Element (1) length: " &
                    Integer'Image (Integer (Y.Element (1).Length)));

        Put_Line
          ("Decision_Tree_Classifer.Fit Num_Samples, Num_Outputs: " &
             Tree.Index_Range'Image (aClassifier.Attributes.Num_Features) &
             Tree.Index_Range'Image (aClassifier.Attributes.Num_Outputs));

        Classifier_Utilities.Print_Value_List
          ("Decision_Tree_Classifer.Fit X (1)", X.Element (1));
        Classifier_Utilities.Print_Value_List
          ("Decision_Tree_Classifer.Fit X (2)", X.Element (2));
        Classifier_Utilities.Print_Value_List
          ("Decision_Tree_Classifer.Fit Y (1)", Y.Element (1));

        --  L293
        if Num_Labels /= Num_Samples then
            raise Classifier_Error with
              "Decision_Tree_Classifer.Fit Number of labels =" &
              Integer'Image (Num_Labels) & " does not match number of samples ="
              & Integer'Image (Num_Samples);
        end if;

        --  L206
        Classification_Fit (aClassifier, Y, Y_Encoded, Expanded_Class_Weight);
        Put_Line ("Decision_Tree_Classifer.Fit after Classification_Fit Y length: " &
                    Integer'Image (Integer (Y.Length)));
        Classifier_Utilities.Print_Value_List
          ("Decision_Tree_Classifer.Fit after Classification_Fit Y (1)",
           Y.Element (1));
        Classifier_Utilities.Print_Float_List
          ("Decision_Tree_Classifer.Fit Expanded_Class_Weight",
           Expanded_Class_Weight);
        --  L218

        --  L226
        Max_Leaf_Nodes := aClassifier.Parameters.Max_Leaf_Nodes;

        Check_Parameters;

        --  L318
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

        --  L350
        Ada_Tree_Build.Build_Tree (theTree, X, Y, Sample_Weight);
        --  L410

    end Fit;

    --  -------------------------------------------------------------------------

    procedure Init (aClassifier    : in out Classifier;
                    Max_Leaf_Nodes : Integer := -1;
                    Random_State   : Integer := 0) is
    begin
        aClassifier.Parameters.Random_State := Random_State;
        aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
    end Init;

    --  -------------------------------------------------------------------------
    --  Based on class.py fit L431 Predict
    --  Prune tree using Minimal Cost-Complexity Pruning.
    function Predict (Self : in out Classifier;
                      X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List is
    begin
        return Tree.Predict (Self.Attributes.Decision_Tree, X);
    end Predict;

    --  -------------------------------------------------------------------------

    --  Based on class.py fit L545 _prune_tree
    --  Prune tree using Minimal Cost-Complexity Pruning.
    procedure Prune_Tree (Self : in out Classifier) is
    begin
        null;
    end Prune_Tree;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
