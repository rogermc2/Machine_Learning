--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Assertions;  use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Types;
--  with Printing;

package body Node_Splitter is

   Feature_Threshold : constant Float := 10.0 ** (-7);

   procedure Evaluate_All_Splits (Splitter   : in out Splitter_Class;
                                  Current    : in out Split_Record;
                                  Best       : in out Split_Record);
   procedure Process (Splitter              : in out Splitter_Class;
                      Num_Total_Constants   : in out Natural;
                      Num_Found_Constants   : in out Natural;
                      Start_Row, Stop_Row   : Positive;
                      F_I                   : in out Natural; F_J : Natural;
                      Best_Split            : in out Split_Record);
   procedure Update_Constants (Self                  : in out Splitter_Class;
                               Num_Known_Constants   : Natural;
                               Num_Found_Constants   : Natural);

   --  -------------------------------------------------------------------------

   procedure C_Init (Self             : in out Splitter_Class;
                     Max_Features     : Tree.Index_Range := 1;
                     Min_Leaf_Samples : Integer := 0;
                     Min_Leaf_Weight  : Float := 0.0) is
   begin
      Self.Max_Features := Max_Features;
      Self.Min_Leaf_Samples := Min_Leaf_Samples;
      Self.Min_Leaf_Weight := Min_Leaf_Weight;
   end C_Init;

   --  -------------------------------------------------------------------------

   function Can_Split (Self                  : in out Splitter_Class;
                       Num_Total_Constants   : Natural;
                       F_I, F_J              : Natural)
                       return Boolean is
      use ML_Types;
      Routine_Name : constant String := "Node_Splitter.Can_Split";
      X_F_Start    : constant Value_Record :=
                       Self.Feature_Values.Element (Self.Feature_Values.First_Index);
      X_F_End      : constant Value_Record :=
                       Self.Feature_Values.Element (Self.Feature_Values.Last_Index);
      Swap         : Natural;
      LE           : Boolean;
      OK           : Boolean := False;
   begin
      --  L363
      case X_F_Start.Value_Kind is
         when Float_Type =>
            LE := X_F_End.Float_Value <= X_F_Start.Float_Value +
              Feature_Threshold;

         when Integer_Type =>
            LE := X_F_End.Integer_Value <= X_F_Start.Integer_Value;

         when others =>
            raise Node_Splitter_Error with Routine_Name & ", invalid X_Features"
              & ML_Types.Data_Type'Image (X_F_Start.Value_Kind);
      end case;

      --  Still L363
      if LE then
         Swap := Self.Feature_Indices.Element (F_J);
         Self.Feature_Indices.Replace_Element
           (F_J, Self.Feature_Indices.Element
              (Num_Total_Constants + 1));
         Self.Feature_Indices.Replace_Element
           (Num_Total_Constants + 1, Swap);

      else  --  L378
         OK := not LE and F_I > 1;
      end if;

      return OK;

   end Can_Split;

   --  -------------------------------------------------------------------------
   --  L373
   procedure Evaluate_All_Splits (Splitter   : in out Splitter_Class;
                                  Current    : in out Split_Record;
                                  Best       : in out Split_Record) is
      use ML_Types;
      Routine_Name              : constant String :=
                                    "Node_Splitter.Evaluate_All_Splits ";
      Features_X                : constant ML_Types.Value_Data_List :=
                                    Splitter.Feature_Values;
      P_Index                   : Positive;
      Current_Proxy_Improvement : Float := -Float'Last;
      Best_Proxy_Improvement    : Float := -Float'Last;
      Best_Updated              : Boolean := False;

      function Compare (Features   : ML_Types.Value_Data_List;
                        P_Index    : Positive) return Boolean is
         use ML_Types;
         XP     : constant Value_Record := Features.Element (P_Index);
         XP_1   : constant Value_Record := Features.Element (P_Index + 1);
         Result : Boolean;
      begin
         --  Check for next feature value equal to or less than
         --  current feature value
         Result := XP_1.Value_Kind = XP.Value_Kind;
         if Result then
            case XP.Value_Kind is
            when Boolean_Type =>
               Result := not XP_1.Boolean_Value and XP.Boolean_Value;

            when Float_Type =>
               Result := XP_1.Float_Value <= XP.Float_Value +
                 Feature_Threshold;

            when Integer_Type =>
               Result := XP_1.Integer_Value <= XP.Integer_Value;

            when UB_String_Type =>
               Result := XP_1.UB_String_Value <= XP.UB_String_Value;
            end case;
         end if;

         return Result;

      end Compare;

   begin
      --  Set of features to be split:
      --  Splitter.Start_Index through Splitter.End_Index
      --  L380 Evaluate all splits
      P_Index := Splitter.Start_Row;
      while P_Index < Splitter.Stop_Row loop
         --  L383
         while P_Index + 1 < Splitter.Stop_Row and
           Compare (Features_X, P_Index) loop
            --  L380
            P_Index := P_Index + 1;
         end loop;

         --  L389
         P_Index := P_Index + 1;
         --  L393
         if P_Index <= Splitter.Stop_Row then
            Current.Split_Row := P_Index;
            --  Best.Pos_I is the start index of the right node's data
            --  L397 Accept if min_samples_leaf is guaranteed
            if Current.Split_Row - Splitter.Start_Row >=
              Splitter.Min_Leaf_Samples and
              Splitter.Stop_Row - Current.Split_Row >=
                Splitter.Min_Leaf_Samples then
               --  L400
               Criterion.Update (Splitter.Criteria, Current.Split_Row);

               --  L402 Accept if min_weight_leaf is satisfied
               if Splitter.Criteria.Num_Weighted_Left >=
                 Splitter.Min_Leaf_Weight and
                 Splitter.Criteria.Num_Weighted_Right >=
                   Splitter.Min_Leaf_Weight then
                  --  L409  Note: Improvements are negative values.
                  Current_Proxy_Improvement :=
                    Criterion.Proxy_Impurity_Improvement (Splitter.Criteria);
                  if Current_Proxy_Improvement > Best_Proxy_Improvement then
                     Best_Proxy_Improvement := Current_Proxy_Improvement;
                     --  L414
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
                     Best_Updated := True;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if not Best_Updated then
         Put_Line (Routine_Name & " WARNING: Best Fit not updated");
      end if;
      --        Assert (Best_Updated, Routine_Name & "Best Fit was not updated");

      Assert (Best.Split_Row > Splitter.Start_Row,
              "Node_Splitter.Evaluate_All_Splits, split position" &
                Integer'Image (Best.Split_Row) &
                " should be greater than Start_Index" &
                Integer'Image (Splitter.Start_Row));

   end Evaluate_All_Splits;

   --  -------------------------------------------------------------------------
   ---  BestSplitter.Find_Best_Split samples up to max_features without
   --  replacement using a Fisher-Yates-based algorithm.
   --  Variables F_I and F_J are used to compute a permutation of the Features
   --  being classified.
   procedure Find_Best_Split (Self                  : in out Splitter_Class;
                              Num_Constant_Features : Natural;
                              Num_Found_Constants   : in out Natural;
                              Num_Total_Constants   : in out Natural;
                              Best_Split            : in out Split_Record) is
      Routine_Name         : constant String :=
                               "Node_Splitter.Find_Best_Split";
      Num_Features         : constant Natural :=
                               Natural (Self.Feature_Indices.Length);
      Num_Known_Constants  : constant Natural := Num_Constant_Features;
      Max_Features         : constant Tree.Index_Range := Self.Max_Features;
      Start_Row            : constant Positive := Self.Start_Row;
      Stop_Row             : constant Positive := Self.Stop_Row;
      Num_Drawn_Constants  : Natural := 0;
      Num_Visited_Features : Natural := 0;
      F_I                  : Natural := Num_Features;
      F_J                  : Natural;
      Swap                 : Natural;
   begin
      Assert (Stop_Row >= Start_Row, Routine_Name & " Stop_Row " &
                Integer'Image (Stop_Row) &
                " should be greater than Start_Row "
              & Integer'Image (Start_Row));
      --  L323
      while F_I > Num_Total_Constants + 1 and
        (Num_Visited_Features < Positive (Max_Features) or
           --   At least one drawn feature must be a non-constant.
           Num_Visited_Features <=
             Num_Found_Constants + Num_Drawn_Constants) loop
         --  L329
         Num_Visited_Features := Num_Visited_Features + 1;
         --  L342
         --  Draw a feature at random
         --  F_J = random integer with 2 <= F_J <= F_I
         F_J := 2 +
           Maths.Random_Integer * (F_I - 2 - Num_Found_Constants);

         if F_J > Num_Known_Constants then
            --  L349 F_J > Num_Known_Constants
            --  F_J is in the interval
            --  Num_Known_Constants .. F_I - Num_Found_Constants
            F_J := F_J + Num_Found_Constants;
            --  F_J is in the interval Num_ Total_Constants .. F_I
            Process (Self, Num_Total_Constants, Num_Found_Constants, Start_Row,
                     Stop_Row, F_I, F_J, Best_Split);
         else --  L346
            --  F_J is a constant in the interval
            --  Num_Drawn_Constants ..  Num_Found_Constants
            Swap := Self.Feature_Indices.Element (Num_Drawn_Constants + 1);
            Self.Feature_Indices.Replace_Element
              (Num_Drawn_Constants + 1, Self.Feature_Indices.Element (F_J));
            Self.Feature_Indices.Replace_Element (F_J, Swap);
            Num_Drawn_Constants := Num_Drawn_Constants + 1;
         end if;
      end loop;  --  L430

   end Find_Best_Split;

   --  -------------------------------------------------------------------------
   --  L116
   procedure Init (Self             : in out Splitter_Class;
                   Input_X          : ML_Types.Value_Data_Lists_2D;
                   Y_Encoded        : Classifier_Types.Natural_Lists_2D;
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

      --  L146 Create a new list which will store nonzero samples from the
      --  feature of interest
      Self.Sample_Indices.Clear;
      --  L152 For each sample (row)
      for index_i in Input_X.First_Index .. Input_X.Last_Index loop
         --  Only work with positively weighted samples.
         if Sample_Weight.Is_Empty or else
           Sample_Weight.Element (index_i) > 0.0 then
            Self.Sample_Indices.Append (index_i);
         end if;

         if Sample_Weight.Is_Empty then
            Weighted_Samples := Weighted_Samples + 1.0;
         else
            Weighted_Samples :=
              Weighted_Samples + Sample_Weight.Element (index_i);
         end if;
      end loop;

      --  Number of samples is the number of positively weighted samples.
      Self.Num_Samples := Positive (Self.Sample_Indices.Length);
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
   --  L42
   procedure Init_Split (theSplit : in out Split_Record; Stop : Positive) is
   begin
      theSplit.Split_Row := Stop;
      theSplit.Impurity_Left := Float'Last;
      theSplit.Impurity_Right := Float'Last;
      theSplit.Improvement := -Float'Last;
      theSplit.Feature := 1;
      theSplit.Threshold := 0.0;

   end Init_Split;

   --  -------------------------------------------------------------------------

   function Entropy_Node_Impurity (Self : Splitter_Class) return Float is
   begin
      return Criterion.Node_Impurity_Entropy (Self.Criteria);
   end Entropy_Node_Impurity;

   --  -------------------------------------------------------------------------

   function Gini_Node_Impurity (Self : Splitter_Class) return Float is
   begin
      return Criterion.Node_Impurity_Gini (Self.Criteria);
   end Gini_Node_Impurity;

   --  -------------------------------------------------------------------------

   procedure Node_Value (Self   : Splitter_Class;
                         Values : out Weights.Weight_Lists_2D) is
   begin
      Criterion.Node_Value (Self.Criteria, Values);
   end Node_Value;

   --  -------------------------------------------------------------------------

   procedure Process (Splitter              : in out Splitter_Class;
                      Num_Total_Constants   : in out Natural;
                      Num_Found_Constants   : in out Natural;
                      Start_Row, Stop_Row   : Positive;
                      F_I                   : in out Natural; F_J : Natural;
                      Best_Split            : in out Split_Record) is
      use ML_Types;
      use Value_Data_Sorting;
      --        Routine_Name         : constant String := "Node_Splitter.Process ";
      Current_Split        : Split_Record;
      X_Samples_Row        : Natural;
      X_Samples            : Value_Data_List;
      Swap                 : Natural;
   begin
      Current_Split := Best_Split;
      --  L354 Sort samples along Current.Feature index;
      Current_Split.Feature := Splitter.Feature_Indices.Element (F_J);
      --  L358
      for index in Start_Row .. Stop_Row loop
         X_Samples_Row := Splitter.Sample_Indices.Element (index);
         X_Samples := Splitter.X.Element (X_Samples_Row);
         --  Splitter.Feature_Values is a Value_Data_List
         Splitter.Feature_Values.Replace_Element
           (index, X_Samples (Current_Split.Feature));
      end loop;

      --  L361
      Sort (Splitter.Feature_Values);
      --  Splitter.Feature_Values is a value_data_list
      if Can_Split (Splitter, Num_Total_Constants, F_I, F_J) then
         --  L375
         F_I := F_I - 1;
         Swap := Splitter.Feature_Indices.Element (F_I);
         Splitter.Feature_Indices.Replace_Element
           (F_I, Splitter.Feature_Indices.Element (F_J));
         Splitter.Feature_Indices.Replace_Element (F_J, Swap);

         --  L379 Reset the criterion to pos = start
         Criterion.Reset (Splitter.Criteria);

         Evaluate_All_Splits (Splitter, Current_Split, Best_Split);
         --  L428
      else -- L370
         --           Put_Line (Routine_Name & "L370 can't split");
         Num_Found_Constants := Num_Found_Constants + 1;
         Num_Total_Constants := Num_Total_Constants + 1;
      end if;

   end Process;

   --  -------------------------------------------------------------------------

   procedure Reorder_Rows (Self        : in out Splitter_Class;
                           Best_Split  : in out Split_Record;
                           X_Samples   : in out
                             Classifier_Types.Natural_List;
                           Impurity    : Float) is
      use Classifier_Types.Natural_Package;
      Routine_Name  : constant String := "Node_Splitter.Reorder_Rows ";
      Partition_End : Natural;
      P_Index       : Positive;
      Sample_PI     : Positive;
      X_1           : ML_Types.Value_Data_List;
      X             : ML_Types.Value_Record;
      Swap          : Positive;
   begin
      --  L417 Reorganize into samples[start:best.pos] + samples[best.pos:end]
      if Best_Split.Split_Row <= Self.Stop_Row then
         --  L419
         Partition_End := Self.Stop_Row;
         P_Index := Self.Start_Row;
         while P_Index < Partition_End loop
            Sample_PI := Self.Sample_Indices.Element (P_Index);
            X_1 := Self.X.Element (Sample_PI);
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
                  raise Node_Splitter_Error with Routine_Name &
                    "Node_Splitter.Reorder_Rows X.Value_Kind is invalid";
            end case;
         end loop;

         --  L436
         Criterion.Reset (Self.Criteria);
         Criterion.Update (Self.Criteria, Best_Split.Split_Row);
         Criterion.Gini_Children_Impurity
           (Self.Criteria, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
         Best_Split.Improvement := Criterion.Impurity_Improvement
           (Self.Criteria, Impurity, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
      end if;

   end Reorder_Rows;

   --  -------------------------------------------------------------------------
   --  Reset_Node resets the splitter Split based on node Split.Samples[start:end].
   procedure Reset_Node
     (Splitter              : in out Splitter_Class;
      Start_Row, Stop_Row   : Positive;
      Weighted_Node_Samples : in out Float) is
      Routine_Name : constant String := "Node_Splitter.Reset_Node";
   begin
      Assert (Stop_Row >= Start_Row,
              Routine_Name & " stop index" & Integer'Image (Stop_Row) &
                " should not be less than start index" &
                Integer'Image (Start_Row));

      Splitter.Start_Row := Start_Row;
      Splitter.Stop_Row := Stop_Row;
      Criterion.Classification_Init
        (Splitter.Criteria, Splitter.Y, Splitter.Sample_Indices,
         Splitter.Sample_Weight, Splitter.Weighted_Samples,
         Start_Row, Stop_Row);

      Weighted_Node_Samples := Splitter.Criteria.Num_Weighted_Node_Samples;

   end Reset_Node;

   --  -------------------------------------------------------------------------
   --  L268 BestSplitter.Split_Node implements a Fisher-Yates-based algorithm
   --  in the Find_Best_Split procedure.
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record is
      Routine_Name         : constant String := "Node_Splitter.Split_Node";
      Num_Known_Constants  : constant Natural := Num_Constant_Features;
      Num_Total_Constants  : Natural := Num_Known_Constants;
      Num_Found_Constants  : Natural := 0;
      Best_Split           : Split_Record;
   begin
      --  L277 samples is a pointer to self.samples
      --  L281 features is a pointer to self.features
      --  L282 constant_features is a pointer to self.constant_features
      --  L285 Xf is a pointer to self.feature_values
      Assert (not Self.Sample_Indices.Is_Empty, Routine_Name &
                " called with empty Sample_Indices");
      --  L308
      Init_Split (Best_Split, Self.Stop_Row);
      --  L319
      Find_Best_Split (Self, Num_Constant_Features, Num_Found_Constants,
                       Num_Total_Constants, Best_Split);

      --  L417  Reorganize into samples
      --        (start .. best.pos) + samples (best.pos .. end)
      Reorder_Rows (Self, Best_Split, Self.Sample_Indices, Impurity);
      Update_Constants (Self, Num_Known_Constants, Num_Found_Constants);

      --  L454
      Num_Constant_Features := Num_Total_Constants;

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
