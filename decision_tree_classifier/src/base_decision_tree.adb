--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Ada_Tree_Builder;
with Classifier_Utilities;
with Criterion;
with Encode_Utils;
--  with Printing;
with Utilities;

package body Base_Decision_Tree is

   procedure Base_Fit_Checks
     (aClassifier       : in out Classifier;
      X                 : ML_Types.Value_Data_Lists_2D;
      Y                 : Classifier_Types.Natural_Lists_2D;
      Min_Samples_Split : out Positive;
      Sample_Weights    : in out Classifier_Types.Float_List);
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y_Orig                 : ML_Types.Value_Data_Lists_2D;
      Y_Encoded              : out Classifier_Types.Natural_Lists_2D;
      Classes                : out ML_Types.Value_Data_Lists_2D;
      Expanded_Class_Weights : in out Classifier_Types.Float_List);
   procedure Prune_Tree (aClassifier : in out Classifier);

   --  -------------------------------------------------------------------------
   --  if is_classification: part of _classes.py BasesDecisionTree.Fit
   --  L150
   procedure Base_Fit
     (aClassifier    : in out Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y_Orig         : ML_Types.Value_Data_Lists_2D;
      Sample_Weights : out Weights.Weight_List) is
      use Ada.Containers;
      use Estimator;
      Routine_Name          : constant String :=
                                "Base_Decision_Tree.Base_Fit ";
      Num_Samples           : constant Positive := Positive (X.Length);
      Y_Encoded             : Classifier_Types.Natural_Lists_2D;
      Classes               : ML_Types.Value_Data_Lists_2D;
      Min_Samples_Split     : Positive := 1;
      --  L205
      Expanded_Class_Weight : Weights.Weight_List;
      Min_Weight_Leaf       : Float := 0.0;
      Sum_Sample_Weight     : Float := 0.0;
   begin
      Assert (not X.Is_Empty, Routine_Name & ", X is empty");
      Assert (not Y_Orig.Is_Empty, Routine_Name & ", Y is empty");
      if not Sample_Weights.Is_Empty then
         Assert (Sample_Weights.Length = X.Length, Routine_Name &
                   " Sample_Weights must be the same size as X.");
      end if;

      --  X is 2D list num samples x num features
      --  Y_Orig is 2D list num classes x num outputs
      --  L207  Generates Y_Encoded and Classes
      if aClassifier.Estimator_Kind = Classifier_Estimator then
         Classification_Part (aClassifier, Y_Orig, Y_Encoded,
                              Classes, Expanded_Class_Weight);
      end if;

      --  L189
      aClassifier.Attributes.Num_Features :=
        Tree.Index_Range (X.Element (1).Length);

      --  L229
      Base_Fit_Checks (aClassifier, X, Y_Encoded, Min_Samples_Split, Sample_Weights);
      --  Base_Fit_Checks ends at L350

      Node_Splitter.C_Init
        (aClassifier.Parameters.Splitter,
         Tree.Index_Range (aClassifier.Attributes.Max_Features),
         aClassifier.Parameters.Min_Samples_Leaf, Min_Weight_Leaf);
      Criterion.C_Init (aClassifier.Parameters.Splitter.Criteria,
                        aClassifier.Attributes.Num_Outputs,
                        aClassifier.Attributes.Decision_Tree.Num_Classes);
      --  L323
      if not Expanded_Class_Weight.Is_Empty then
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

      --  L333
      if Sample_Weights.Is_Empty then
         Min_Weight_Leaf := aClassifier.Parameters.Min_Weight_Fraction_Leaf
           * Float (Num_Samples);
      else
         Sum_Sample_Weight := 0.0;
         for index in Sample_Weights.First_Index ..
           Sample_Weights.Last_Index loop
            Sum_Sample_Weight :=
              Sum_Sample_Weight + Sample_Weights.Element (index);
         end loop;
         Min_Weight_Leaf := aClassifier.Parameters.Min_Weight_Fraction_Leaf
           * Sum_Sample_Weight;
      end if;

      aClassifier.Parameters.Splitter.Min_Leaf_Weight := Min_Weight_Leaf;

      --  L392
      Tree.C_Init (aClassifier.Attributes.Decision_Tree,
                   Positive (aClassifier.Attributes.Num_Features),
                   aClassifier.Attributes.Decision_Tree.Num_Classes,
                   aClassifier.Attributes.Num_Outputs);

      aClassifier.Attributes.Decision_Tree.Classes :=
        aClassifier.Attributes.Classes;

      --  L410
      Ada_Tree_Builder.Build_Tree
        (aClassifier.Attributes.Decision_Tree,
         aClassifier.Parameters.Splitter, X, Y_Encoded,
         Sample_Weights, Min_Samples_Split,
         aClassifier.Parameters.Min_Samples_Leaf, Min_Weight_Leaf,
         aClassifier.Parameters.Max_Depth,
         aClassifier.Parameters.Min_Impurity_Decrease);

      Prune_Tree (aClassifier);

   end Base_Fit;

   --  -------------------------------------------------------------------------

   procedure Base_Fit_Checks
     (aClassifier       : in out Classifier;
      X                 : ML_Types.Value_Data_Lists_2D;
      Y                 : Classifier_Types.Natural_Lists_2D;
      Min_Samples_Split : out Positive;
      Sample_Weights    : in out Classifier_Types.Float_List) is
      --        use Maths.Float_Math_Functions;
      use Tree;
      Routine_Name      : constant String :=
                            "Base_Decision_Tree.Base_Fit_Checks";
      Num_Samples       : constant Positive := Positive (X.Length);
      --  L229
      Max_Depth         : Natural;
      Max_Leaf_Nodes    : constant Integer :=
                            aClassifier.Parameters.Max_Leaf_Nodes;
      Max_Features      : Index_Range := Tree.Index_Range'Last;
      --        Sqrt_Num_Features : Index_Range := 1;
   begin
      --  L229
      if aClassifier.Parameters.Max_Depth < 0 then
         Max_Depth := Integer'Last;
         aClassifier.Parameters.Max_Depth := Max_Depth;
      else
         Max_Depth := aClassifier.Parameters.Max_Depth;
      end if;

      --  L235
      Assert (aClassifier.Parameters.Min_Samples_Leaf > 0,
              Routine_Name & ", Min_Samples_Leaf must be at least 1");

      --  L250
      case aClassifier.Parameters.Min_Samples_Split.Value_Kind is
         when Split_Float =>
            Assert (aClassifier.Parameters.Min_Samples_Split.Float_Value > 0.0
                    and
                      aClassifier.Parameters.Min_Samples_Split.Float_Value <= 1.0,
                    Routine_Name &
                      " Min_Samples_Split must be in the range (0.0, 1.0]");
            Min_Samples_Split :=
              Integer (Float'Ceiling
                       (aClassifier.Parameters.Min_Samples_Split.Float_Value *
                              Float (Num_Samples)));
            Min_Samples_Split := Integer'Max (2, Min_Samples_Split);

         when Split_Integer =>
            Assert (aClassifier.Parameters.Min_Samples_Split.Integer_Value > 1,
                    Routine_Name & " Min_Samples_Split must be at least 2");
            Min_Samples_Split :=
              aClassifier.Parameters.Min_Samples_Split.Integer_Value;
      end case;

      --  L268
      Min_Samples_Split := Integer'Max
        (Min_Samples_Split, 2  * aClassifier.Parameters.Min_Samples_Leaf);

      --        Sqrt_Num_Features :=
      --          Tree.Index_Range (Sqrt (Float (aClassifier.Attributes.Num_Features)));
      --        if Sqrt_Num_Features > 1 then
      --           Max_Features := Sqrt_Num_Features;
      --        else
      --           Max_Features := 1;
      --        end if;

      --  L288 Integral Max_Features
      Max_Features := aClassifier.Parameters.Max_Features;
      --  L291
      aClassifier.Parameters.Max_Features := Max_Features;

      --  L301
      Assert (Positive (Y.Length) = Num_Samples, Routine_Name &
                ", number of labels " & Integer'Image (Integer (Y.Length)) &
                " does not match number of samples " &
                Integer'Image (Num_Samples));

      --  L306
      Assert (aClassifier.Parameters.Min_Weight_Fraction_Leaf >= 0.0 and
                aClassifier.Parameters.Min_Weight_Fraction_Leaf < 5.0,
              Routine_Name & ", Min_Weight_Fraction_Leaf is " &
                Float'Image (aClassifier.Parameters.Min_Weight_Fraction_Leaf)
              &  " but should be in (0.0, 5.0]");

      --  L308
      Assert (Max_Depth > 0, Routine_Name & ", must be greater than 0.");

      --  L310
      if Max_Features <= 0 then
         --          Max_Features > aClassifier.Attributes.Num_Features then
         raise Value_Error with Routine_Name &
           ", Max_Features " & Index_Range'Image (Max_Features) &
           " is not in (0, Num_Features)";
      end if;

      if Max_Features > aClassifier.Attributes.Num_Features then
         aClassifier.Parameters.Max_Features :=
           aClassifier.Attributes.Num_Features;
      end if;

      --  L316
      Assert (Max_Leaf_Nodes = -1 or Max_Leaf_Nodes > 1, Routine_Name &
                ", Max_Leaf_Nodes " & Integer'Image (Max_Leaf_Nodes) &
                "must be > 1");

      --  L323
      if not Sample_Weights.Is_Empty then
         Assert (Integer (Sample_Weights.Length) = Num_Samples,
                 Routine_Name & ", Sample_Weight length " &
                   "should be the same as the number of X samples");
      end if;
      --  L350
      Assert (aClassifier.Parameters.Min_Impurity_Decrease >= 0.0,
              Routine_Name & ", Min_Impurity_Decrease should be >= 0");

   end Base_Fit_Checks;

   --  -------------------------------------------------------------------------

   procedure C_Init (aClassifier              : in out Classifier;
                     Min_Samples_Split        : String;
                     Criterion_Type           : Criterion.Classifier_Criteria_Type :=
                       Criterion.Gini_Criteria;
                     Min_Leaf_Samples         : Integer := 1;
                     Max_Features             : Tree.Index_Range :=
                       Tree.Index_Range'Last;
                     Class_Weight             : Weights.Weight_Type :=
                       Weights.No_Weight;
                     Max_Depth                : Integer := -1;
                     Min_Weight_Fraction_Leaf : Float := 0.0;
                     Max_Leaf_Nodes           : Integer := -1;
                     Min_Impurity_Decrease    : Float := 0.0;
                     CCP_Alpha                : Float := 0.0;
                     Random_State             : Integer := 0) is
   begin
      if Utilities.Is_Float (To_Unbounded_String (Min_Samples_Split)) then
         declare
            Min_Split : Split_Value_Record (Split_Float);
         begin
            Min_Split.Float_Value := Float'Value (Min_Samples_Split);
            aClassifier.Parameters.Min_Samples_Split := Min_Split;
         end;
      elsif Utilities.Is_Integer
        (To_Unbounded_String (Min_Samples_Split)) then
         declare
            Min_Split : Split_Value_Record (Split_Integer);
         begin
            Min_Split.Integer_Value := Integer'Value (Min_Samples_Split);
            aClassifier.Parameters.Min_Samples_Split := Min_Split;
         end;

      end if;

      aClassifier.Parameters.Criterion_Kind := Criterion_Type;
      aClassifier.Parameters.Max_Depth := Max_Depth;
      aClassifier.Parameters.Min_Samples_Leaf := Min_Leaf_Samples;
      aClassifier.Parameters.Min_Weight_Fraction_Leaf :=
        Min_Weight_Fraction_Leaf;
      aClassifier.Parameters.Max_Features := Max_Features;
      aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
      aClassifier.Parameters.Min_Impurity_Decrease := Min_Impurity_Decrease;
      aClassifier.Parameters.Class_Weight := Class_Weight;
      aClassifier.Parameters.CCP_Alpha := CCP_Alpha;
      aClassifier.Parameters.Random_State := Random_State;
      Node_Splitter.C_Init (aClassifier.Parameters.Splitter);

   end C_Init;

   --  -------------------------------------------------------------------------
   --  based on L200 of _classes.py BasesDecisionTree.Fit
   procedure Classification_Part
     (aClassifier             : in out Classifier;
      Y_Orig                  : ML_Types.Value_Data_Lists_2D;
      Y_Encoded               : out Classifier_Types.Natural_Lists_2D;
      Classes                 : out ML_Types.Value_Data_Lists_2D;
      Expanded_Class_Weights  : in out Weights.Weight_List) is
      use Ada.Containers;
      use Classifier_Types;
      use ML_Types;
      use Weights;
      Routine_Name : constant String :=
                       "Base_Decision_Tree.Classification_Part ";
      Num_Outputs  : constant Count_Type := Y_Orig.Element (1).Length;
      Y_Row        : Value_Data_List := Value_Data_Package.Empty_Vector;
      Yk_Row       : Value_Data_List := Value_Data_Package.Empty_Vector;
      YE_Row       : Natural_List := Natural_Package.Empty_Vector;
      OP_Row       : Value_Data_List := Value_Data_Package.Empty_Vector;
      Column       : Natural_List := Natural_Package.Empty_Vector;
      Inverse      : Natural_List := Natural_Package.Empty_Vector;
      Class_List   : ML_Types.Value_Data_List;
   begin
      aClassifier.Attributes.Classes.Clear;
      aClassifier.Attributes.Decision_Tree.Num_Classes.Clear;
      Y_Encoded.Clear;
      Classes.Clear;
      Y_Encoded.Set_Length (Y_Orig.Length);
      aClassifier.Attributes.Num_Outputs := Tree.Index_Range (Num_Outputs);

      --  Y is 2D list num samples x num outputs
      --  Y_Encoded is 2D list num samples x num outputs
      --  L215  Initialize Y_Encoded
      for class in Y_Orig.First_Index .. Y_Orig.Last_Index loop
         Column.Clear;
         for op in Y_Orig.Element (1).First_Index
           .. Y_Orig.Element (1).Last_Index loop
            Column.Append (op);
         end loop;
         Y_Encoded.Replace_Element (class, Column);
      end loop;

      --  Classes is 2D list num outputs x num classes
      OP_Row.Set_Length (Num_Outputs);
      for op in Y_Orig.Element (1).First_Index
        .. Y_Orig.Element (1).Last_Index loop
         Yk_Row.Clear;
         for class in Y_Orig.First_Index .. Y_Orig.Last_Index loop
            Y_Row := Y_Orig.Element (class);
            Yk_Row.Append (Y_Row.Element (op));
         end loop;
         Class_List := (Encode_Utils.Unique (Yk_Row, Inverse));
         aClassifier.Attributes.Decision_Tree.Num_Classes.Append
           (Positive (Class_List.Length));

         for class in Y_Orig.First_Index .. Y_Orig.Last_Index loop
            YE_Row := Y_Encoded.Element (class);
            YE_Row.Replace_Element (op, Inverse.Element (class));
            Y_Encoded.Replace_Element (class, YE_Row);
         end loop;
         Classes.Append (Class_List);
      end loop;

      aClassifier.Attributes.Classes := Classes;

      --  L222
      if aClassifier.Parameters.Class_Weight /= No_Weight then
         Expanded_Class_Weights :=
           Weights.Compute_Sample_Weight (No_Weight, Y_Orig);
      end if;
      --  L227
      Classes := aClassifier.Attributes.Classes;
      aClassifier.Parameters.Splitter.Criteria.Num_Classes :=
        aClassifier.Attributes.Decision_Tree.Num_Classes;

   exception
      when others => raise Classifier_Error with Routine_Name & "error";

   end Classification_Part;

   --  -------------------------------------------------------------------------

   function Decision_Path (aClassifier : Classifier;
                           X           : ML_Types.Value_Data_Lists_2D)
                           return Classifier_Types.Natural_Lists_2D is
   begin
      return Tree.Decision_Path (aClassifier.Attributes.Decision_Tree, X);

   end Decision_Path;

   --  -------------------------------------------------------------------------

   function Predict (Self : in out Classifier;
                     X    : ML_Types.Value_Data_Lists_2D)
                      return ML_Types.Value_Data_Lists_2D is
      --  returns num samples x num outputs
      use Weights;
--        Routine_Name      : constant String := "Base_Decision_Tree.Predict";
      --  L468 Prob_A: num_samples x num_outputs x num_classes
      Prob_A            : constant Weight_Lists_3D :=
                            Tree.Predict (Self.Attributes.Decision_Tree, X);
      --  Prob_Ak: num_samples x num_classes
      Prob_Ak           : Weight_Lists_2D;
      Class_Values      : ML_Types.Value_Data_List;
      Samples_2K        : Weight_Lists_2D;
      Samples           : Weight_Lists_2D;
      Classes           : Weight_List;
      Selected_Class    : ML_Types.Value_Record;
      --  Selected_Classes (prediction) 1 x num samples
      Selected_Classes  : ML_Types.Value_Data_List;
      Max_Indices       : Classifier_Types.Natural_List;  --  argmax
      --  Predictions, num outputs x num samples
      Predictions       : ML_Types.Value_Data_Lists_2D;
   begin
      --  L478
      for op in 1 .. Positive (Self.Attributes.Num_Outputs) loop
         Prob_Ak.Clear;
         Selected_Classes.Clear;
         --  Prob_A: num_samples x num_outputs x num_classes
         for sample_index in Prob_A.First_Index .. Prob_A.Last_Index loop
            Samples_2K := Prob_A (sample_index);
            Classes := Samples_2K.Element (op);
            Samples.Append (Classes);
         end loop;

         --  Samples: num_samples x num_classes
         Max_Indices := Classifier_Utilities.Arg_Max (Samples);
         Selected_Classes.Clear;
         Class_Values := Self.Attributes.Classes.Element (op);
         for index in Max_Indices.First_Index .. Max_Indices.Last_Index loop
            Classes := Samples.Element (index);
            Selected_Class :=
              Class_Values.Element (Max_Indices (index));
            Selected_Classes.Append (Selected_Class);
         end loop;

         Predictions.Append (Selected_Classes);
      end loop;

      --  Transposed Predictions, num samples x num outputs
      return Classifier_Utilities.Transpose (Predictions);

   end Predict;

   --  -------------------------------------------------------------------------
   --  Prune tree using Minimal Cost-Complexity Pruning.
   procedure Prune_Tree (aClassifier : in out Classifier) is
   begin
      null;
   end Prune_Tree;

   --  -------------------------------------------------------------------------

end Base_Decision_Tree;
