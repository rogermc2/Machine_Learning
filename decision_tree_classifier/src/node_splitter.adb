--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Assertions;  use Ada.Assertions;
with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Types;
--  with Classifier_Utilities;

package body Node_Splitter is

   Feature_Threshold : constant Float := 10.0 ** (-7);

   procedure Evaluate_All_Splits (Self       : in out Splitter_Class;
                                  Features_X : ML_Types.Value_Data_List;
                                  F_I, F_J   : Natural;
                                  Current    : in out Split_Record;
                                  Best       : in out Split_Record);
   procedure Process (Self                  : in out Splitter_Class;
                      Num_Total_Constants   : in out Natural;
                      Num_Found_Constants   : in out Natural;
                      Start_Row, End_Row    : Positive;
                      F_I                   : in out Natural; F_J : Natural;
                      Best_Split            : in out Split_Record);
   procedure Update_Constants (Self                  : in out Splitter_Class;
                               Num_Known_Constants   : Natural;
                               Num_Found_Constants   : Natural);

   --  -------------------------------------------------------------------------

   procedure C_Init (Self             : in out Splitter_Class;
                     Criteria         : Criterion.Criterion_Class;
                     Max_Features     : Tree.Index_Range := 1;
                     Min_Leaf_Samples : Positive := 1;
                     Min_Leaf_Weight  : Float := 0.0) is
   begin
      Self.Criteria := Criteria;
      Self.Max_Features := Max_Features;
      Self.Min_Leaf_Samples := Min_Leaf_Samples;
      Self.Min_Leaf_Weight := Min_Leaf_Weight;
   end C_Init;

   --  -------------------------------------------------------------------------

   procedure Check_For_Split (Self                  : in out Splitter_Class;
                              Num_Total_Constants   : in out Natural;
                              Num_Found_Constants   : in out Natural;
                              F_I                   : in out Natural; F_J : Natural;
                              Current_Split         : in out Split_Record;
                              Best_Split            : in out Split_Record) is
      use ML_Types;
      X_F_Start : Value_Record;
      X_F_End   : Value_Record;
      Swap      : Natural;
      LE        : Boolean;
   begin
      X_F_Start :=
        Self.Feature_Values.Element (Self.Feature_Values.First_Index);
      X_F_End :=
        Self.Feature_Values.Element (Self.Feature_Values.Last_Index);

      case X_F_Start.Value_Kind is
         when Float_Type =>
            LE := X_F_End.Float_Value  <= X_F_Start.Float_Value +
              Feature_Threshold;

         when Integer_Type =>
            LE := X_F_End.Integer_Value <= X_F_Start.Integer_Value;

         when others =>
            raise Node_Splitter_Error with
              "Node_Splitter.Check_For_Split, invalid X_Features" &
              ML_Types.Data_Type'Image (X_F_Start.Value_Kind);
      end case;

      --  Still L369
      if LE then
         Swap := Self.Feature_Indices.Element (F_J);
         Self.Feature_Indices.Replace_Element
           (F_J, Self.Feature_Indices.Element
              (Num_Total_Constants + 1));
         Self.Feature_Indices.Replace_Element
           (Num_Total_Constants + 1, Swap);

         Num_Found_Constants := Num_Found_Constants + 1;
         Num_Total_Constants := Num_Total_Constants + 1;

      elsif F_I > 1 then  --  L378
         F_I := F_I - 1;
         Evaluate_All_Splits (Self, Self.Feature_Values, F_I, F_J,
                              Current_Split, Best_Split);
         --  L428
         --                      Put_Line ("Node_Splitter.Check_For_Split found");
      end if;
   end Check_For_Split;

   --  -------------------------------------------------------------------------

   procedure Evaluate_All_Splits (Self       : in out Splitter_Class;
                                  Features_X : ML_Types.Value_Data_List;
                                  F_I, F_J   : Natural;
                                  Current    : in out Split_Record;
                                  Best       : in out Split_Record) is
      use ML_Types;
      P_Index                   : Positive;
      Current_Proxy_Improvement : Float := -Float'Last;
      Best_Proxy_Improvement    : Float := -Float'Last;
      Swap                      : Natural;
      LE                        : Boolean;
   begin
      Swap := Self.Feature_Indices.Element (F_I);
      Self.Feature_Indices.Replace_Element
        (F_I, Self.Feature_Indices.Element (F_J));
      Self.Feature_Indices.Replace_Element (F_J, Swap);

      --  L377 Evaluate all splits
      --                      Put_Line ("Node_Splitter.Evaluate_All_Splits L382 Start_Row, End_Row " &
      --                                  Integer'Image (Start_Row) & Integer'Image (End_Row));
      --                      Classifier_Utilities.Print_Value_Data_List
      --                        ("Node_Splitter.Evaluate_All_Splits L382, Self.Feature_Values",
      --                         Self.Feature_Values);
      --  L384 Reset the criterion to pos = start
      Criterion.Reset (Self.Criteria);
      P_Index := Self.Start_Row;

      --          Put_Line ("Node_Splitter.Evaluate_All_Splits L384, start, end: " &
      --                      Integer'Image (Self.Start_Row) &
      --                      Integer'Image (Self.End_Row));
      --          Put_Line ("Node_Splitter.Evaluate_All_Splits Features_X start, end" &
      --                      Integer'Image (Integer (Features_X.First_Index)) &
      --                      Integer'Image (Integer (Features_X.Last_Index)));
      Best := Current;

      --  Set of features to be split :
      --  P_Index: Self.Start_Index through Self.End_Index
      while P_Index < Self.End_Row loop
         --              Put_Line ("Node_Splitter.Evaluate_All_Splits P_Index " &
         --                          Integer'Image (P_Index));
         LE := True;
         while P_Index + 1 < Self.End_Row and LE loop
            --                  Put_Line ("Node_Splitter.Evaluate_All_Splits P_Index + 1:" &
            --                              Integer'Image (P_Index + 1));
            case Features_X.Element (P_Index + 1).Value_Kind is
               when Boolean_Type =>
                  --  if current X value is false and next X value is true
                  --  increment X index
                  LE := not Features_X.Element (P_Index + 1).Boolean_Value and
                    Features_X.Element (P_Index + 1).Boolean_Value;
                  if LE then
                     P_Index := P_Index + 1;
                  end if;

               when Float_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  LE := Features_X.Element (P_Index + 1).Float_Value <=
                    Features_X.Element
                      (P_Index).Float_Value + Feature_Threshold;
                  if LE then
                     P_Index := P_Index + 1;
                  end if;

               when Integer_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  LE := Features_X.Element (P_Index + 1).Integer_Value <=
                    Features_X.Element (P_Index).Integer_Value;
                  if LE then
                     P_Index := P_Index + 1;
                  end if;

               when UB_String_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  LE := Features_X.Element (P_Index + 1).UB_String_Value <=
                    Features_X.Element (P_Index).UB_String_Value;
                  if LE then
                     P_Index := P_Index + 1;
                  end if;
                  --                      Put_Line ("Node_Splitter.Evaluate_All_Splits P_Index after case " &
                  --                                  Integer'Image (P_Index));
            end case;
         end loop; --  P1_Index

         P_Index := P_Index + 1;
         --  L393
         --              Put_Line ("Node_Splitter.Find_BeEvaluate_All_Splitsst_Split L393");
         if P_Index < Self.End_Row then
            Best.Split_Row := P_Index;
            --  Best.Pos_I is the start index of the right node's data
            --  L398 Accept if min_samples_leaf is guaranteed
            if Current.Split_Row - Self.Start_Row >= Self.Min_Leaf_Samples and
              Self.End_Row - Current.Split_Row >= Self.Min_Leaf_Samples then
               --  L400
               Criterion.Update (Self.Criteria, Best.Split_Row);

               --  L402 Accept if min_weight_leaf is satisfied
               if Self.Criteria.Num_Weighted_Left >= Self.Min_Leaf_Weight and
                 Self.Criteria.Num_Weighted_Right >= Self.Min_Leaf_Weight then
                  Current_Proxy_Improvement :=
                    Criterion.Proxy_Impurity_Improvement (Self.Criteria);
                  --  L409  Note: Improvements are negative values.
                  --                          Put_Line ("Node_Splitter.Find_Best_Split L412");
                  if Current_Proxy_Improvement > Best_Proxy_Improvement then
                     Best_Proxy_Improvement := Current_Proxy_Improvement;
                     --  L41
                     --                              Put_Line ("Node_Splitter.Evaluate_All_Splits L415");
                     case Features_X.Element (P_Index).Value_Kind is
                        when Float_Type =>
                           Current.Threshold := 0.5 *
                             (Features_X.Element (P_Index - 1).Float_Value
                              + Features_X.Element (P_Index).Float_Value);
                           if Current.Threshold =
                             Features_X.Element (P_Index).Float_Value or
                             Current.Threshold = Float'Last or
                             Current.Threshold = (-Float'Last) then
                              case Features_X.Element
                                (P_Index - 1).Value_Kind is
                                 when Float_Type =>
                                    Current.Threshold := Features_X.Element
                                      (P_Index - 1).Float_Value;
                                 when Integer_Type =>
                                    Current.Threshold :=
                                      Float (Features_X.Element
                                             (P_Index - 1).Integer_Value);
                                 when Boolean_Type =>
                                    if Features_X.Element
                                      (P_Index - 1).Boolean_Value then
                                       Current.Threshold := 1.0;
                                    else
                                       Current.Threshold := 0.0;
                                    end if;
                                 when UB_String_Type => null;
                              end case;
                           end if;

                        when Integer_Type =>
                           Current.Threshold := 0.5 * Float
                             (Features_X.Element (P_Index - 1).Integer_Value
                              + Features_X.Element (P_Index).Integer_Value);
                           if Current.Threshold =
                             Float (Features_X.Element (P_Index).
                                        Integer_Value) or
                               Current.Threshold = Float'Last or
                               Current.Threshold = (-Float'Last) then
                              Current.Threshold :=
                                Float (Features_X.Element (P_Index - 1).
                                           Integer_Value);
                           end if;

                        when Boolean_Type | UB_String_Type => null;
                     end case;

                     --  L419
                     Best := Current;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if Best.Split_Row <= Self.Start_Row then
         raise Node_Splitter_Error with
           "Node_Splitter.Evaluate_All_Splits, final position" &
           Integer'Image (Best.Split_Row) &
           " should be greater than Start_Index" &
           Integer'Image (Self.Start_Row);
      end if;

   end Evaluate_All_Splits;

   --  -------------------------------------------------------------------------

   procedure Find_Best_Split (Self                  : in out Splitter_Class;
                              Num_Features          : Natural;
                              Num_Constant_Features : Natural;
                              Num_Found_Constants   : in out Natural;
                              Num_Total_Constants   : in out Natural) is

      Num_Known_Constants  : constant Natural := Num_Constant_Features;
      Max_Features         : constant Tree.Index_Range := Self.Max_Features;
      Start_Row            : constant Positive := Self.Start_Row;
      End_Row              : constant Positive := Self.End_Row;
      Num_Drawn_Constants  : Natural := 0;
      Num_Visited_Features : Natural := 0;
      F_I                  : Natural := Num_Features;
      F_J                  : Natural;
      Swap                 : Natural;
      Best_Split           : Split_Record;
   begin
      Assert (End_Row > Start_Row, "Node_Splitter.Find_Best_Split End_Row " &
                Integer'Image (End_Row) & " should be greater than Start_Row "
              & Integer'Image (Start_Row));

      while F_I > Num_Total_Constants and
        (Num_Visited_Features < Natural (Max_Features) or
           --   At least one drawn feature must be non constant.
           Num_Visited_Features <=
             Num_Found_Constants + Num_Drawn_Constants) loop
         --  L329
         Num_Visited_Features := Num_Visited_Features + 1;
         --  L342
         --  Draw a feature at random;
         F_J := Num_Drawn_Constants + 1 +
           Maths.Random_Integer mod (F_I - Num_Found_Constants);
         --  L346
         if F_J <= Num_Known_Constants then
            Num_Drawn_Constants := Num_Drawn_Constants + 1;
            Swap := Self.Feature_Indices.Element (Num_Drawn_Constants);
            Self.Feature_Indices.Replace_Element
              (Num_Drawn_Constants, Self.Feature_Indices.Element (F_J));
            Self.Feature_Indices.Replace_Element (F_J, Swap);
         else  --  L356
            F_J := F_J + Num_Found_Constants;
            if F_J = 0 then
               F_J := 1;
            end if;
            Process (Self, Num_Total_Constants, Num_Found_Constants, Start_Row,
                     End_Row, F_I, F_J, Best_Split);
         end if;
      end loop;  --  L430

   end Find_Best_Split;

   --  -------------------------------------------------------------------------

   procedure Init (Self             : in out Splitter_Class;
                   Input_X          : ML_Types.Value_Data_Lists_2D;
                   Y_Encoded        : Classifier_Types.List_Of_Natural_Lists;
                   Sample_Weight    : Weights.Weight_List;
                   Min_Leaf_Samples : Positive := 1) is
      use Ada.Containers;
      Num_Samples      : constant Positive := Positive (Input_X.Length);
      Weighted_Samples : Float := 0.0;
   begin
      Self.Min_Leaf_Samples := Min_Leaf_Samples;
      --  X dimensions: num samples x num features
      Self.Max_Features := Tree.Index_Range (Input_X.Element (1).Length);
      --  Min_Leaf_Samples is the minimum number of samples that each leaf can
      --  have, where splits which would result in having less samples in a
      --  leaf are not considered.
      Self.Sample_Indices.Clear;
      --  For each sample (row)
      for index in Input_X.First_Index .. Input_X.Last_Index loop
         --  Only work with positively weighted samples.
         if Sample_Weight.Is_Empty or else
           Sample_Weight.Element (index) > 0.0 then
            Self.Sample_Indices.Append (index);
         end if;

         if Sample_Weight.Is_Empty then
            Weighted_Samples := Weighted_Samples + 1.0;
         else
            Weighted_Samples :=
              Weighted_Samples + Sample_Weight.Element (index);
         end if;
      end loop;

      --  Number of samples is the number of positively weighted samples.
      Self.Num_Samples := Natural (Self.Sample_Indices.Length);
      Self.Weighted_Samples := Weighted_Samples;

      Self.Feature_Indices.Clear;
      for index in 1 .. Integer (Self.Max_Features) loop
         Self.Feature_Indices.Append (index);
      end loop;

      Self.Feature_Values.Clear;
      Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
      Self.Constant_Features_I.Clear;
      Self.Constant_Features_I.Set_Length (Count_Type (Self.Max_Features));

      Self.X := Input_X;
      Self.Y := Y_Encoded;
      Self.Sample_Weight := Sample_Weight;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Init_Split (theSplit : in out Split_Record; Start : Positive) is
   begin
      theSplit.Split_Row := Start + 1;
      theSplit.Impurity_Left := Float'Last;
      theSplit.Impurity_Right := Float'Last;
      theSplit.Improvement := -Float'Last;
      theSplit.Feature := 1;
      theSplit.Threshold := 0.0;

   end Init_Split;

   --  -------------------------------------------------------------------------

   function Node_Impurity (Self : Splitter_Class) return Float is
   begin
      return Criterion.Entropy_Node_Impurity (Self.Criteria);
   end Node_Impurity;

   --  -------------------------------------------------------------------------

   procedure Node_Value
     (Self   : Splitter_Class;
      Values : out Weights.Weight_Lists_2D) is
   begin
      Criterion.Node_Value (Self.Criteria, Values);
   end Node_Value;

   --  -------------------------------------------------------------------------

   procedure Process (Self                  : in out Splitter_Class;
                      Num_Total_Constants   : in out Natural;
                      Num_Found_Constants   : in out Natural;
                      Start_Row, End_Row    : Positive;
                      F_I                   : in out Natural; F_J : Natural;
                      Best_Split            : in out Split_Record) is
      use ML_Types;
      use Value_Data_Sorting;
      Current_Split        : Split_Record;
      X_Samples_Row        : Natural;
      X_Samples            : Value_Data_List;
   begin
      --  L358 Sort samples along Current.Feature index;
      Current_Split.Feature := Self.Feature_Indices.Element (F_J);
      --  L364
      for index in Start_Row .. End_Row loop
         X_Samples_Row := Self.Sample_Indices.Element (index);
         X_Samples := Self.X.Element (X_Samples_Row);
         --  Self.Feature_Values is a Value_Data_List
         Self.Feature_Values.Replace_Element
           (index, X_Samples (Current_Split.Feature));
      end loop;

      --  L367
      Sort (Self.Feature_Values);
      --  L369  Self.Feature_Values is a value_data_list
      Check_For_Split (Self, Num_Total_Constants, Num_Found_Constants, F_I, F_J,
                       Current_Split, Best_Split );
   end Process;

   --  -------------------------------------------------------------------------

   procedure Reorder_Rows (Self        : in out Splitter_Class;
                           Best_Split  : in out Split_Record;
                           X_Samples   : in out
                             Classifier_Types.Natural_List;
                           Impurity    : Float) is
      use Classifier_Types.Natural_Package;
      Partition_End : Natural;
      P_Index       : Positive;
      Sample_P      : Natural;
      X_1           : ML_Types.Value_Data_List;
      X             : ML_Types.Value_Record;
      Swap          : Positive;
      Crit          : Criterion.Criterion_Class;
   begin
      --  L425 Reorganize into samples[start:best.pos] + samples[best.pos:end]
      if Best_Split.Split_Row < Self.End_Row then
         Partition_End := Self.End_Row;
         P_Index := Self.Start_Row;
         while P_Index < Partition_End loop
            Sample_P := Self.Sample_Indices.Element (P_Index);
            X_1 := Self.X.Element (Sample_P);
            X := X_1.Element (Best_Split.Feature);

            case X.Value_Kind is
               when ML_Types.Float_Type =>
                  if X.Float_Value <=
                    Best_Split.Threshold then
                     P_Index := P_Index + 1;
                  else
                     Partition_End := Partition_End - 1;
                     Swap := X_Samples.Element (P_Index);
                     X_Samples.Replace_Element
                       (P_Index, X_Samples.Element (Partition_End));
                     X_Samples.Replace_Element (Partition_End, Swap);
                  end if;

               when ML_Types.Integer_Type =>
                  if Float (X.Integer_Value) <= Best_Split.Threshold then
                     P_Index := P_Index + 1;
                  else
                     Partition_End := Partition_End - 1;
                     Swap := X_Samples.Element (P_Index);
                     X_Samples.Replace_Element
                       (P_Index, X_Samples.Element (Partition_End));
                     X_Samples.Replace_Element (Partition_End, Swap);
                  end if;

               when others =>
                  raise Node_Splitter_Error with
                    "Node_Splitter.Reorder_Rows X.Value_Kind is invalid";
            end case;
         end loop;

         --  L436
         Criterion.Reset (Self.Criteria);
         Crit := Self.Criteria;
         Crit.Split_Row := Best_Split.Split_Row;
         Criterion.Update (Self.Criteria, Crit.Split_Row);
         Criterion.Gini_Children_Impurity
           (Self.Criteria, Best_Split.Impurity_Left, Best_Split.Impurity_Right);
         Best_Split.Improvement := Criterion.Impurity_Improvement
           (Self.Criteria, Impurity, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
      end if;

   end Reorder_Rows;

   --  -------------------------------------------------------------------------
   --  Reset_Node resets the splitter Split based on node Split.Samples[start:end].
   procedure Reset_Node
     (Splitter              : in out Splitter_Class;
      Start_Row, End_Row    : Positive;
      Weighted_Node_Samples : in out Float) is
   begin
      Assert (End_Row >= Start_Row,
              "Node_Splitter.Reset_Node, stop index" &
                Integer'Image (End_Row) &
                " should not be less than start index" &
                Integer'Image (Start_Row));

      Splitter.Start_Row := Start_Row;
      Splitter.End_Row := End_Row;
      Criterion.Classification_Init
        (Splitter.Criteria, Splitter.Y, Splitter.Sample_Indices,
         Splitter.Sample_Weight, Splitter.Weighted_Samples,
         Start_Row, End_Row);

      Weighted_Node_Samples := Splitter.Criteria.Num_Weighted_Node_Samples;

   end Reset_Node;

   --  -------------------------------------------------------------------------
   --  BestSplitter.Split_Node samples up to max_features without replacement
   --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
   --  and `f_j` to compute a permutation of the `features` array).
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record is
      Num_Features         : constant Natural :=
                               Natural (Self.Feature_Indices.Length);
      Current_Split        : Split_Record;
      Num_Known_Constants  : constant Natural := Num_Constant_Features;
      Num_Total_Constants  : Natural := Num_Known_Constants;
      Num_Found_Constants  : Natural := 0;
      Best_Split           : Split_Record;
   begin
      --  L277 samples is a pointer to self.samples
      --  L281 features is a pointer to self.features
      --  L282 constant_features is a pointer to self.constant_features
      --  L285 Xf is a pointer to self.feature_values
      Assert (not Self.Sample_Indices.Is_Empty,
              "Node_Splitter.Split_Node called with empty Sample_Indices");

      Init_Split (Current_Split, Self.Start_Row);
      --          Classifier_Utilities.Print_Split_Record
      --            ("Node_Splitter.Split_Node Current_Split", Current_Split);
      Find_Best_Split (Self, Num_Features, Num_Constant_Features,
                       Num_Found_Constants, Num_Total_Constants);

      --  L421  Reorganize into samples
      --        (start .. best.pos) + samples (best.pos .. end)
      Reorder_Rows (Self, Best_Split, Self.Sample_Indices, Impurity);
      Update_Constants (Self, Num_Known_Constants, Num_Found_Constants);

      --  L454
      Num_Constant_Features := Num_Total_Constants;
      --  L453
      return Best_Split;

   end Split_Node;

   --  ------------------------------------------------------------------------

   procedure Update_Constants (Self                  : in out Splitter_Class;
                               Num_Known_Constants   : Natural;
                               Num_Found_Constants   : Natural) is
   begin
      --  L442
      --  Respect invariant for constant features: the original order of
      --  element in features[:n_known_constants] must be preserved for
      --  sibling and child nodes.
      if Num_Known_Constants > 0 then
         for index in 1 .. Num_Known_Constants loop
            Self.Feature_Indices.Replace_Element
              (index, Self.Constant_Features_I.Element (index));
         end loop;

         --  L447 Copy newly found constant features
         for index in Num_Known_Constants + 1 .. Num_Found_Constants loop
            Self.Constant_Features_I.Replace_Element
              (index, Self.Feature_Indices.Element (index));
         end loop;
      end if;

   end Update_Constants;

   --  ------------------------------------------------------------------------

end Node_Splitter;
