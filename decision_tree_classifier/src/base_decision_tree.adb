--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Ada_Tree_Builder;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Encode_Utils;

package body Base_Decision_Tree is

   procedure Base_Fit_Checks
     (aClassifier    : in out Classifier;
      X              : ML_Types.List_Of_Value_Data_Lists;
      Y              : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weights : in out Classifier_Types.Float_List);
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y                      : ML_Types.List_Of_Value_Data_Lists;
      Y_Encoded              : out Classifier_Types.List_Of_Natural_Lists;
      Classes                : out ML_Types.List_Of_Value_Data_Lists;
      Expanded_Class_Weights : in out Classifier_Types.Float_List);
   procedure Prune_Tree (aClassifier : in out Classifier);

   --  -------------------------------------------------------------------------
   --  if is_classification: part of _classes.py BasesDecisionTree.Fit
   --  L150
   procedure Base_Fit
     (aClassifier    : in out Classifier;
      X              : ML_Types.List_Of_Value_Data_Lists;
      Y              : ML_Types.List_Of_Value_Data_Lists;
      Y_Encoded      : out Classifier_Types.List_Of_Natural_Lists;
      Classes        : out ML_Types.List_Of_Value_Data_Lists;
      Sample_Weights : out Classifier_Types.Float_List) is
      --  L205
      Expanded_Class_Weight : Weights.Weight_List;
      Sum_Sample_Weight     : Float := 0.0;
   begin
      if Integer (X.Length) < 1 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit, X is empty";
      end if;

      if Integer (Y.Length) < 1 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit, Y is empty";
      end if;

      --  L184
      aClassifier.Attributes.Num_Features :=
        Tree.Index_Range (X.Element (1).Length);
      --  L207
      Classification_Part (aClassifier, Y, Y_Encoded,
                           Classes, Expanded_Class_Weight);

      Base_Fit_Checks (aClassifier, X, Y, Sample_Weights);

      if Expanded_Class_Weight.Is_Empty then
         if Sample_Weights.Is_Empty then
            Sample_Weights := Expanded_Class_Weight;
         else
            for index in Sample_Weights.First_Index ..
              Sample_Weights.Last_Index loop
               Sample_Weights.Replace_Element
                 (index, Sample_Weights.Element (index) *
                      Expanded_Class_Weight.Element (index));
            end loop;
         end if;
      end if;

      --  L325
      if not Sample_Weights.Is_Empty then
         Sum_Sample_Weight := 0.0;
         for index in Sample_Weights.First_Index ..
           Sample_Weights.Last_Index loop
            Sum_Sample_Weight := Sum_Sample_Weight + Sample_Weights.Element (index);
         end loop;
      end if;

      aClassifier.Attributes.Decision_Tree.Classes :=
        aClassifier.Attributes.Classes;
      --  L410
      --        Classifier_Utilities.Print_List_Of_Natural_Lists
      --          ("Base_Decision_Tree.Base_Fit, Y_Encoded", Y_Encoded);
      Ada_Tree_Builder.Build_Tree (aClassifier.Attributes.Decision_Tree,
                                   X, Y_Encoded, Sample_Weights);

      if Integer (aClassifier.Attributes.Num_Outputs) = 1 then
         null;
      end if;

      Prune_Tree (aClassifier);

   end Base_Fit;

   --  -------------------------------------------------------------------------

   procedure Base_Fit_Checks
     (aClassifier    : in out Classifier;
      X              : ML_Types.List_Of_Value_Data_Lists;
      Y              : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weights : in out Classifier_Types.Float_List) is
      use Maths.Float_Math_Functions;
      Num_Samples           : constant Positive := Positive (X.Length);
      --  L226
      Max_Leaf_Nodes        : constant Integer := aClassifier.Parameters.Max_Leaf_Nodes;
      Min_Sample_Leaf       : Leaf_Record (Tree.Integer_Type);
      Min_Sample_Split      : Split_Record (Tree.Integer_Type);
      Max_Features          : Tree.Features_Record (Tree.Integer_Type);
      Sqrt_Num_Features     : Natural;
   begin
      --  L235
      case aClassifier.Parameters.Min_Samples_Leaf.Leaf_Type is
         when Tree.Integer_Type =>
            if aClassifier.Parameters.Min_Samples_Leaf.Min_Leaf < 1 then
               raise Value_Error with
                 "Base_Decision_Tree.Base_Fit_Checks, Min_Samples_Leaf must be at least 1";
            end if;
            Min_Sample_Leaf.Min_Leaf := aClassifier.Parameters.Min_Samples_Leaf.Min_Leaf;

         when Tree.Float_Type =>
            --  L243
            if aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf <= 0.0 or
              aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf > 0.5 then
               raise Value_Error with
                 "Base_Decision_Tree.Base_Fit_Checks, Min_Fraction_Leaf must be in (0.0, 0.5]";
            end if;
            Min_Sample_Leaf.Min_Leaf :=
              Integer (Float'Ceiling
                       (aClassifier.Parameters.Min_Samples_Leaf.Min_Fraction_Leaf));
         when others =>
            raise Value_Error with
              "Base_Decision_Tree.Base_Fit_Checks, invalid Min_Samples_Leaf Leaf_Type";
      end case;

      --  L250
      case aClassifier.Parameters.Min_Samples_Split.Split_Type is
         when Tree.Integer_Type =>
            if aClassifier.Parameters.Min_Samples_Split.Min_Split < 2 then
               raise Value_Error with
                 "Base_Decision_Tree.Base_Fit_Checks, Min_Samples_Split must be at least 2";
            end if;
            Min_Sample_Split.Min_Split := aClassifier.Parameters.Min_Samples_Split.Min_Split;
         when Tree.Float_Type =>
            --  L253
            if aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split <= 0.0 or
              aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split > 1.0 then
               raise Value_Error with
                 "Base_Decision_Tree.Base_Fit_Checks, Min_Fraction_Split must be in (0.0, 1.0]";
            end if;
            --  L260
            Min_Sample_Split.Min_Split :=
              Integer (Float'Ceiling
                       (aClassifier.Parameters.Min_Samples_Split.Min_Fraction_Split));
            if Min_Sample_Split.Min_Split < 2 then
               Min_Sample_Split.Min_Split := 2;
            end if;
         when others =>
            raise Value_Error with
              "Base_Decision_Tree.Base_Fit_Checks, invalid Min_Samples_Split Split_Type";
      end case;

      --  L263
      if Min_Sample_Split.Min_Split < 2 * Min_Sample_Leaf.Min_Leaf then
         Min_Sample_Split.Min_Split := 2 * Min_Sample_Leaf.Min_Leaf;
      end if;

      Sqrt_Num_Features :=
        Natural (Sqrt (Float (aClassifier.Attributes.Num_Features)));
      if Sqrt_Num_Features > 1 then
         Max_Features.Max_Features := Sqrt_Num_Features;
      else
         Max_Features.Max_Features := 1;
      end if;

      --  L291
      aClassifier.Parameters.Max_Features := Max_Features;

      if Positive (Y.Length) /= Num_Samples then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, number of labels " &
           Integer'Image (Integer (Y.Length)) &
           " does not match number of samples " & Integer'Image (Num_Samples);
      end if;

      --  L298
      if aClassifier.Parameters.Min_Weight_Fraction_Leaf < 0.0 or
        aClassifier.Parameters.Min_Weight_Fraction_Leaf > 5.0 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Weight_Fraction_Leaf is "
           & Float'Image (aClassifier.Parameters.Min_Weight_Fraction_Leaf) &
           " but should be in (0.0, 5.0]";
      end if;

      if Max_Features.Max_Features <= 0 or
        Max_Features.Max_Features > Integer (aClassifier.Attributes.Num_Features) then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Max_Features is not in (0, Num_Features)";
      end if;

      --  L305
      if Max_Leaf_Nodes /= -1 and Max_Leaf_Nodes < 2 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Max_Leaf_Nodes should be > 1";
      end if;
      --  L315
      if not Sample_Weights.Is_Empty then
         if Integer (Sample_Weights.Length) /= Num_Samples then
            raise Value_Error with
              "Base_Decision_Tree.Base_Fit_Checks, Sample_Weight lenghth " &
              "should be the same as the number of X samples";
         end if;
      end if;

      --  L350
      if aClassifier.Parameters.Min_Impurity_Decrease < 0.0 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Impurity_Decrease should be >= 0";
      end if;

   end Base_Fit_Checks;

   --  -------------------------------------------------------------------------
   --  based on L200 of _classes.py BasesDecisionTree.Fit
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y                      : ML_Types.List_Of_Value_Data_Lists;
      Y_Encoded              : out Classifier_Types.List_Of_Natural_Lists;
      Classes                : out ML_Types.List_Of_Value_Data_Lists;
      Expanded_Class_Weights : in out Classifier_Types.Float_List) is
      use Weights;
      Y_K        : ML_Types.Value_Data_List;
      Classes_K  : ML_Types.Value_Data_List;
      Column     : Natural_List;
      Inverse    : Natural_List;
   begin
      aClassifier.Attributes.Classes.Clear;
      Y_Encoded.Clear;
      Classes.Clear;
      Y_Encoded.Set_Length (Y.Length);

      --  L208  Initialize Y_Encoded
      for row in Y.First_Index .. Y.Last_Index loop
         Column.Clear;
         for col in Y.Element (1).First_Index .. Y.Element (1).Last_Index loop
            Column.Append (col);
         end loop;
         Y_Encoded.Replace_Element (row, Column);
      end loop;

      for row in Y_Encoded.First_Index .. Y_Encoded.Last_Index loop
         Column.Clear;
         Y_K := Y.Element (row);
         for col in Y.Element (1).First_Index .. Y.Element (1).Last_Index loop
            Classes_K := Encode_Utils.Unique (Y_K, Inverse);
            Y_Encoded.Replace_Element (row, Inverse);
            aClassifier.Attributes.Classes.Append (Classes_K);
            if Classes.Is_Empty then
               Classes.Append (Classes_K);

            else
               for class_row in Classes.First_Index .. Classes.Last_Index loop
                  if Classes.Element (class_row).Is_Empty then
                     Classes.Replace_Element (class_row, Classes_K);
                  end if;
               end loop;
            end if;

         end loop;
      end loop;

      Classifier_Utilities.Print_List_Of_Natural_Lists
        ("Base_Decision_Tree.Classification_Part Y_Encoded", Y_Encoded);
      Classifier_Utilities.Print_List_Of_Value_Lists  ("Base_Decision_Tree.Classification_Part Classes", Classes);

      --  L218
      if aClassifier.Parameters.Class_Weight /= No_Weight then
         Expanded_Class_Weights :=
           Weights.Compute_Sample_Weight (No_Weight, Y);
      end if;

      Criterion.Init (aClassifier.Parameters.Critera,
                      aClassifier.Attributes.Classes);
      Classes := aClassifier.Attributes.Classes;

   end Classification_Part;

   --  -------------------------------------------------------------------------

   procedure Init (aClassifier              : in out Classifier;
                   Max_Leaf_Nodes           : Integer := -1;
                   Min_Weight_Fraction_Leaf : Float := 0.0;
                   Random_State             : Integer := 0) is
   begin
      aClassifier.Parameters.Random_State := Random_State;
      aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
      aClassifier.Parameters.Min_Weight_Fraction_Leaf :=
        Min_Weight_Fraction_Leaf;
      aClassifier.Parameters.Min_Samples_Split.Min_Split := 2;
      aClassifier.Parameters.Min_Samples_Leaf.Min_Leaf := 1;
   end Init;

   --  -------------------------------------------------------------------------
   --  Based on class.py fit L431 Predict
   function Predict (Self : in out Classifier;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List is
   begin
      return Tree.Predict (Self.Attributes.Decision_Tree, X);
   end Predict;

   --  -------------------------------------------------------------------------
   --  Prune tree using Minimal Cost-Complexity Pruning.
   procedure Prune_Tree (aClassifier : in out Classifier) is
   begin
      null;
   end Prune_Tree;

   --  -------------------------------------------------------------------------

end Base_Decision_Tree;
