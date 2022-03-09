--  Based on scikit-learn/sklearn/tree/_classes.py class
--  DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Depth_First_Builder;
with Best_First_Builder;
with Classifier_Utilities;
with Encode_Utils;
with Printing;
with Tree_Build;
with Utilities;

package body Base_Decision_Tree is

   procedure Base_Fit_Checks
     (aClassifier       : in out Classifier;
      X                 : Float_List_2D;
      Y                 : Natural_List;
      Min_Samples_Split : out Positive;
      Sample_Weights    : in out Float_List);
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y                      : Integer_List;
      Y_Encoded              : out Natural_List;
      Classes                : out Integer_List;
      Expanded_Class_Weights : in out Float_List);
   procedure Prune_Tree (aClassifier : in out Classifier);

   --  -------------------------------------------------------------------------

   function Apply (aClassifier : Classifier; X : Float_List_2D)
                   return Natural_List is
   begin

      return Tree.Apply (aClassifier.Attributes.Decision_Tree, X);

   end Apply;

   --  -------------------------------------------------------------------------
   --  if is_classification: part of _classes.py BasesDecisionTree.Fit L150
   procedure Base_Fit
     (aClassifier    : in out Classifier;
      X              : Float_List_2D;
      Y              : Integer_List;
      Sample_Weights : out Weights.Weight_List) is
      use Ada.Containers;
      use Estimator;
      Routine_Name          : constant String :=
                                "Base_Decision_Tree.Base_Fit ";
      Num_Samples           : constant Positive := Positive (X.Length);
      Builder               : Tree_Build.Tree_Builder;
      Y_Encoded             : Natural_List;
      Classes               : Integer_List;
      Min_Samples_Split     : Positive := 1;
      --  L205
      Expanded_Class_Weight : Weights.Weight_List;
      Min_Weight_Leaf       : Float := 0.0;
      Sum_Sample_Weight     : Float := 0.0;
   begin
      Assert (not X.Is_Empty, Routine_Name & ", X is empty");
      Assert (not Y.Is_Empty, Routine_Name & ", Y is empty");
      if not Sample_Weights.Is_Empty then
         Assert (Sample_Weights.Length = X.Length, Routine_Name &
                   " Sample_Weights must be the same size as X.");
      end if;

      --  X is 2D list num samples x num features Y is 2D list num classes
      --  x num outputs L207 Generates Y_Encoded and Classes
      if aClassifier.Estimator_Kind = Classifier_Estimator then
         Classification_Part (aClassifier, Y, Y_Encoded,
                              Classes, Expanded_Class_Weight);
      end if;
--        Put_Line (Routine_Name & "L189 Classes length: " &
--                  Integer'Image (Integer (Classes.Length)));

      --  L189
      aClassifier.Attributes.Num_Features :=
        Tree.Index_Range (X.Element (1).Length);

      --  L229
      Base_Fit_Checks (aClassifier, X, Y_Encoded, Min_Samples_Split,
                       Sample_Weights);
      --  Base_Fit_Checks ends at L350

--        Put_Line (Routine_Name & "Num_Classes " &
--                  Integer'Image (aClassifier.Attributes.Decision_Tree.Num_Classes));
      Node_Splitter.C_Init
        (aClassifier.Parameters.Splitter,
         Tree.Index_Range (aClassifier.Attributes.Max_Features),
         aClassifier.Parameters.Min_Samples_Leaf, Min_Weight_Leaf);
      Criterion.C_Init (aClassifier.Parameters.Splitter.Criteria,
                        --                          aClassifier.Attributes.Num_Outputs,
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

      --  L379
      Node_Splitter.Initialize_Splitter
        (aClassifier.Parameters.Splitter, X, Y_Encoded, Sample_Weights);

      aClassifier.Parameters.Splitter.Min_Leaf_Weight := Min_Weight_Leaf;

      --  L392
      Tree.C_Init (aClassifier.Attributes.Decision_Tree,
                   Positive (aClassifier.Attributes.Num_Features),
                   aClassifier.Attributes.Decision_Tree.Num_Classes);

      aClassifier.Attributes.Decision_Tree.Classes :=
        aClassifier.Attributes.Classes;

      --  L400
      Tree_Build.Init_Builder (Builder, aClassifier.Parameters.Max_Leaf_Nodes,
                               aClassifier.Parameters.Splitter);

      --  L420
      case Builder.Tree_Kind is
         when Tree_Build.Depth_First_Tree =>
            Depth_First_Builder.Build_Tree
              (aClassifier.Attributes.Decision_Tree, Builder, Y_Encoded);
         when Tree_Build.Best_First_Tree =>
            Best_First_Builder.Build_Tree
              (Builder, aClassifier.Attributes.Decision_Tree);
      end case;

      --  L426
      Prune_Tree (aClassifier);

   end Base_Fit;

   --  -------------------------------------------------------------------------

   procedure Base_Fit_Checks
     (aClassifier       : in out Classifier;
      X                 : Float_List_2D;
      Y                 : Natural_List;
      Min_Samples_Split : out Positive;
      Sample_Weights    : in out Float_List) is
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

      --  L313
      Assert (Max_Leaf_Nodes = -1 or Max_Leaf_Nodes > 1, Routine_Name &
                ", Max_Leaf_Nodes " & Integer'Image (Max_Leaf_Nodes) &
                "must be > 1");

      --  L320
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
      elsif Utilities.Is_Integer (To_Unbounded_String (Min_Samples_Split)) then
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
      Y                       : Integer_List;
      Y_Encoded               : out Natural_List;
      Classes                 : out Integer_List;
      Expanded_Class_Weights  : in out Weights.Weight_List) is
--        use Ada.Containers;
      use Weights;
      Routine_Name : constant String :=
                       "Base_Decision_Tree.Classification_Part ";
   begin
      aClassifier.Attributes.Classes.Clear;
      Y_Encoded.Clear;

      --  L217
      Classes := Encode_Utils.Unique (Y, Y_Encoded);
      aClassifier.Attributes.Classes := Classes;
      aClassifier.Attributes.Decision_Tree.Num_Classes :=
        Positive (Classes.Length);

--        Put_Line (Routine_Name & "Classes length: " &
--                  Integer'Image (Integer (Classes.Length)));

      --  L222
      if aClassifier.Parameters.Class_Weight /= No_Weight then
         Expanded_Class_Weights :=
           Weights.Compute_Sample_Weight (No_Weight, Y);
      end if;
      --  L227
      Classes := aClassifier.Attributes.Classes;
      aClassifier.Parameters.Splitter.Criteria.Num_Classes :=
        aClassifier.Attributes.Decision_Tree.Num_Classes;

   exception
      when others => raise Classifier_Error with Routine_Name & "error";

   end Classification_Part;

   --  -------------------------------------------------------------------------

   function Decision_Path (aClassifier : Classifier; X : Float_List_2D)
                           return Natural_List is
   begin
      return Tree.Decision_Path (aClassifier.Attributes.Decision_Tree, X);

   end Decision_Path;

   --  -------------------------------------------------------------------------

   function Predict (Self : in out Classifier; X : Float_List_2D)
                     return Integer_List is
      use Weights;
      Routine_Name      : constant String := "Base_Decision_Tree.Predict ";
      Prob_A            : constant Weight_Lists_2D :=
                            Tree.Predict (Self.Attributes.Decision_Tree, X);
      Class_Values      : Integer_List;
      Samples           : Weight_Lists_2D;
      Classes           : Weight_List;
      Selected_Class    : Integer;
      --  Selected_Classes (prediction) 1 x num samples
      Selected_Classes  : Integer_List;
      Max_Indices       : Natural_List;  --  argmax
      Predictions       : Integer_List;
   begin
      --  L478
      Selected_Classes.Clear;
      --  Prob_A: num_samples x num_classes
      for sample_index in Prob_A.First_Index .. Prob_A.Last_Index loop
         Classes := Prob_A (sample_index);
         Samples.Append (Classes);
      end loop;

      Put_Line (Routine_Name & "Samples length" &
                  Integer'Image (Integer (Samples.Length)));
      --  Samples: num_samples x num_classes
      Max_Indices := Classifier_Utilities.Arg_Max (Samples);
      Selected_Classes.Clear;
      Class_Values := Self.Attributes.Classes;
      Printing.Print_Integer_List (Routine_Name & "Class_Values", Class_Values);
      for index in Max_Indices.First_Index .. Max_Indices.Last_Index loop
         Classes := Samples.Element (index);
         Selected_Class :=
           Class_Values.Element (Max_Indices (index));
         Selected_Classes.Append (Selected_Class);
      end loop;
      Put_Line (Routine_Name & "loop done");

      Predictions.Append (Selected_Classes);

      return Predictions;

   end Predict;

   --  -------------------------------------------------------------------------
   --  Prune tree using Minimal Cost-Complexity Pruning.
   procedure Prune_Tree (aClassifier : in out Classifier) is
   begin
      null;
   end Prune_Tree;

   --  -------------------------------------------------------------------------

end Base_Decision_Tree;
