--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Text_IO; use Ada.Text_IO;

--  with Builder;
with Classifier_Types;
with Classifier_Utilities;
with Encode_Utils;
with Tree_Build;

package body Decision_Tree_Classifer is

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
      --             Criterion : Classifier_Criteria_Type := Self.Parameters.Criterion;
      Splitter              : Node_Splitter.Splitter_Class := Self.Parameters.Splitter;
      theTree               : Tree.Tree_Class;
      Max_Leaf_Nodes        : Natural := 0;
      Min_Impurity_Decrease : Float := 0.0;
      --          Feature_Values : Value_Data_List;
   begin
      --  L387
      --  if is_classifier(self):
      Self.Attributes.Decision_Tree := theTree;

      --  L388
      theTree.Num_Features := Natural (Self.Attributes.Num_Features);
      theTree.Num_Outputs := Self.Attributes.Num_Outputs;
      Self.Attributes.Decision_Tree := theTree;

      --  L398
      if Self.Parameters.Max_Leaf_Nodes < 0 then
         declare
            Builder : Tree_Build.Tree_Builder (Tree_Build.Depth_First_Tree);
         begin
            --  L419  Depth First case
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
   procedure Classification_Fit
     (aClassifier           : in out Classifier;
      Y                     : in out ML_Types.List_Of_Value_Data_Lists;
      Num_Outputs           : Positive;
      Y_Encoded             : out ML_Types.List_Of_Value_Data_Lists;
      Expanded_Class_Weight : out Classifier_Types.Float_List) is
      use ML_Types;
      use Weights;
      Y_Original : ML_Types.List_Of_Value_Data_Lists;
      Y_K        : Value_Data_List;
      Classes_K  : Value_Data_List;
      Inverse    : Natural_List;
   begin
      --  L206
      Y_Encoded.Clear;
      Y_Encoded.Set_Length (Ada.Containers.Count_Type (Num_Outputs));
      for k in 1 .. Integer (Y.Length) loop
         Y_K := Y.Element (k);
         Classes_K := Encode_Utils.Unique (Y_K, Inverse);
         Y_Encoded.Replace_Element (k, Classes_K);
         aClassifier.Attributes.Classes.Append (Classes_K);
      end loop;
      Y := Y_Encoded;
      Classifier_Utilities.Print_Value_List
        ("Decision_Tree_Classifer.Classification_Fit Y (1)", Y.Element (1));

      Put_Line ("Decision_Tree_Classifer.Classification_Fit Class_Weight: " &
                  Weights.Weight_Type'Image (aClassifier.Parameters.Class_Weight));
      if aClassifier.Parameters.Class_Weight /= Weights.No_Weight then
         Expanded_Class_Weight :=
           Weights.Compute_Sample_Weight (Weights.No_Weight, Y_Original);
      end if;

      --  L215
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
   procedure Fit (aClassifier   : in out Classifier;
                  X             : ML_Types.List_Of_Value_Data_Lists;
                  Y             : in out ML_Types.List_Of_Value_Data_Lists;
                  Sample_Weight : in out Classifier_Types.Weight_List;
                  Check_Input   : Boolean := False) is
      use Ada.Containers;
      use ML_Types;
      use Classifier_Types;
      use Classifier_Types.Integer_Package;
      Num_Samples           : constant Positive :=
                                Positive (X.Element (1).Length);
      Num_Outputs           : constant Positive :=
                                Positive (Y.Element (1).Length);
      --        Random_State          : Integer := aClassifier.Parameters.Random_State;
      Expanded_Class_Weight : Classifier_Types.Float_List;
      Y_Encoded             : List_Of_Value_Data_Lists;
      Max_Leaf_Nodes        : Natural := 0;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
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
      if Positive (Num_Outputs) /= Num_Samples then
         raise Value_Error with
           "Decision_Tree_Classifer.Fit Number of labels =" &
           Integer'Image (Num_Outputs) & " does not match number of samples ="
           & Integer'Image (Num_Samples);
      end if;

      --  L206
      Classification_Fit (aClassifier, Y, Num_Outputs, Y_Encoded,
                          Expanded_Class_Weight);
      Classifier_Utilities.Print_Value_List
        ("Decision_Tree_Classifer.Fit after Classification_Fit Y (1)", Y.Element (1));
      Classifier_Utilities.Print_Float_List
        ("Decision_Tree_Classifer.Fit Expanded_Class_Weight",
         Expanded_Class_Weight);
      --  L218

      --  L226
      if aClassifier.Parameters.Max_Leaf_Nodes > 0 then
         Max_Leaf_Nodes := aClassifier.Parameters.Max_Leaf_Nodes;
      end if;

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
      Build_Tree (aClassifier, Min_Samples_Split, Min_Samples_Leaf,
                  Min_Weight_Leaf, Max_Depth, X, Y, Sample_Weight);
      --  L410

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Init (aClassifier    : in out Classifier;
                   Max_Leaf_Nodes : Positive;
                   Random_State   : Integer) is
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
