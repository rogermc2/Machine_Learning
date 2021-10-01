--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Ada_Tree_Build;
with Base_Decision_Tree;
with Classifier_Types;
with Encode_Utils;
with Node_Splitter;
with Tree_Build;
with Weights;

package body Decision_Tree_Classifer is
   use Base_Decision_Tree;

    procedure Classification_Part
      (aClassifier : in out Classifier;
       Y, Y_Encoded : in out ML_Types.List_Of_Value_Data_Lists;
       Expanded_Class_Weight : in out Classifier_Types.Float_List);
    procedure Prune_Tree (Self : in out Classifier);

    --  -------------------------------------------------------------------------
    --  Based on class.py fit L350 Build tree
    procedure Build_Tree (Self              : in out Base_Decision_Tree.Classifier;
                          Min_Samples_Split : Natural;
                          Min_Samples_Leaf  : Natural;
                          Min_Weight_Leaf   : Float;
                          Max_Depth         : Natural;
                          X, Y              : ML_Types.List_Of_Value_Data_Lists;
                          Sample_Weight     : Classifier_Types.Weight_List) is
        use Tree;
        Max_Leaf_Nodes        : constant Integer :=
                                  Self.Parameters.Max_Leaf_Nodes;
        --  L370
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
    --  L200
    procedure Classification_Part
      (aClassifier : in out Classifier;
       Y, Y_Encoded : in out ML_Types.List_Of_Value_Data_Lists;
       Expanded_Class_Weight : in out Classifier_Types.Float_List) is
        use Weights;
        Y_Local    : ML_Types.List_Of_Value_Data_Lists := Y;
        Y_Original : ML_Types.List_Of_Value_Data_Lists;
        Y_K        : ML_Types.Value_Data_List;
        Classes_K  : ML_Types.Value_Data_List;
        Inverse    : Natural_List;
    begin

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
        Y_Local := Y_Encoded;
        if aClassifier.Parameters.Class_Weight /= No_Weight then
            Expanded_Class_Weight :=
              Weights.Compute_Sample_Weight (No_Weight, Y_Original);
        end if;

    end Classification_Part;

    --  -------------------------------------------------------------------------
    --  L884
    procedure Classification_Fit
      (aClassifier   : in out Base_Decision_Tree.Classifier;
       X             : ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : out Classifier_Types.Float_List;
       Check_Input   : Boolean := False) is
      use Base_Decision_Tree;
    begin
        --  L929
      Base_Fit  (aClassifier, X, Y, Sample_Weight);

        --  L350
      Ada_Tree_Build.Build_Tree (aClassifier.Attributes.Decision_Tree,
                                 X, Y, Sample_Weight);
        --  L410
    end Classification_Fit;

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
