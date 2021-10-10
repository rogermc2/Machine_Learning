--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Classifier_Utilities;

package body Node_Splitter is

   Feature_Threshold : constant Float := 10.0 ** (-7);

   procedure Replacement_Sort (Data        : in out ML_Types.Value_Data_List;
                               Start, Stop : Positive);

   --  -------------------------------------------------------------------------

   procedure Init (Self             : in out Splitter_Class;
                   Input_X          : ML_Types.List_Of_Value_Data_Lists;
                   Y_Encoded        : Classifier_Types.List_Of_Natural_Lists;
                   Sample_Weight    : Weights.Weight_List;
                   Min_Leaf_Samples : Positive := 1) is
      use Ada.Containers;
      Num_Samples      : constant Positive := Positive (Input_X.Length);
      Num_Features     : constant Positive :=
                           Positive (Input_X.Element (1).Length);
      Weighted_Samples : Float := 0.0;
   begin
      Self.Min_Leaf_Samples := Min_Leaf_Samples;
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
      for index in 1 .. Num_Features loop
         Self.Feature_Indices.Append (index);
      end loop;

      Self.Feature_Values.Clear;
      Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
      Self.Constant_Features_I.Clear;
      Self.Constant_Features_I.Set_Length (Count_Type (Num_Features));

      Self.X := Input_X;
      Self.Y := Y_Encoded;
      Self.Sample_Weight := Sample_Weight;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Init_Split (theSplit : in out Split_Record) is
   begin
      theSplit.Impurity_Left := Float'Last;
      theSplit.Impurity_Right := Float'Last;
      theSplit.Improvement := -Float'Last;
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
      Values : out Classifier_Types.List_Of_Float_Lists) is
   begin
      Criterion.Node_Value (Self.Criteria, Values);
   end Node_Value;

   --  -------------------------------------------------------------------------

   procedure Find_Best_Split (Self       : in out Splitter_Class;
                              Features   : Classifier_Types.Natural_List;
                              Features_X : ML_Types.Value_Data_List;
                              Current    : Split_Record;
                              Best       : in out Split_Record) is
      use ML_Types;
      P_Index                   : Natural := Features.First_Index;
      P1_Index                  : Natural;
      Current_Proxy_Improvement : Float := -Float'Last;
      Best_Proxy_Improvement    : Float := -Float'Last;
   begin
      --  L379 Evaluate all splits
      Best := Current;
      Criterion.Reset (Self.Criteria);
      --  Set of features to be split : Self.Start_Index through Self.End_Index
      P1_Index := Self.Start_Index;

      while P_Index <= Self.End_Index loop
         while P1_Index <= Self.End_Index loop
            case Features_X.Element (P1_Index).Value_Kind is
               when Boolean_Type =>
                  --  if current X value is false and next X value is true
                  --  increment X index
                  if not Features_X.Element (P_Index).Boolean_Value and
                    Features_X.Element (P1_Index).Boolean_Value then
                     P_Index := P_Index + 1;
                  end if;
               when Float_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  if Features_X.Element (P1_Index).Float_Value <=
                    Features_X.Element (P_Index).Float_Value + Feature_Threshold then
                     P_Index := P_Index + 1;
                  end if;
               when Integer_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  if Features_X.Element (P1_Index).Integer_Value <=
                    Features_X.Element (P_Index).Integer_Value then
                     P_Index := P_Index + 1;
                  end if;
               when UB_String_Type =>
                  --  if current X is less than or or equal to next X
                  --  increment X index
                  if Features_X.Element (P1_Index).UB_String_Value <=
                    Features_X.Element (P_Index).UB_String_Value then
                     P_Index := P_Index + 1;
                  end if;
            end case;
         end loop; --  P1_Index

         P_Index := P_Index + 1;
         --  L393
         if P_Index < Self.End_Index then
            Best.Pos_I := P_Index;
            --  Best.Pos_I is the start index of the right node's data
            --  L398 Accept if min_samples_leaf is guaranteed
            if Current.Pos_I - Self.Start_Index >= Self.Min_Leaf_Samples and
              Self.End_Index - Current.Pos_I >= Self.Min_Leaf_Samples then
               --  L401
               Criterion.Update (Self.Criteria, Best.Pos_I);

               --  L405 Accept if min_weight_leaf is satisfied
               if Self.Criteria.Weighted_Left >= Self.Min_Leaf_Weight and
                 Self.Criteria.Weighted_Right >= Self.Min_Leaf_Weight then
                  Current_Proxy_Improvement :=
                    Criterion.Proxy_Impurity_Improvement (Self.Criteria);
                  --  L412  Note: Improvements are negative values.
                  if Current_Proxy_Improvement > Best_Proxy_Improvement then
                     Best_Proxy_Improvement := Current_Proxy_Improvement;
                     --  L415
                     case Features_X.Element (P1_Index).Value_Kind is
                        when Float_Type =>
                           Best.Threshold := 0.5 *
                             (Features_X.Element (P_Index - 1).Float_Value
                              + Features_X.Element (P_Index).Float_Value);
                           if Best.Threshold =
                             Features_X.Element (P_Index).Float_Value or
                             Best.Threshold = Float'Last or
                             Best.Threshold = (-Float'Last) then
                              case Features_X.Element
                                (P_Index - 1).Value_Kind is
                                 when Float_Type =>
                                    Best.Threshold := Features_X.Element
                                      (P_Index - 1).Float_Value;
                                 when Integer_Type =>
                                    Best.Threshold := Float (Features_X.Element
                                                             (P_Index - 1).Integer_Value);
                                 when Boolean_Type =>
                                    if Features_X.Element
                                      (P_Index - 1).Boolean_Value then
                                       Best.Threshold := 1.0;
                                    else
                                       Best.Threshold := 0.0;
                                    end if;
                                 when UB_String_Type => null;
                              end case;
                           end if;

                        when Integer_Type =>
                           Best.Threshold := 0.5 * Float
                             (Features_X.Element (P_Index - 1).Integer_Value
                              + Features_X.Element (P_Index).Integer_Value);
                           if Best.Threshold =
                             Float (Features_X.Element (P_Index).
                                        Integer_Value) or
                               Best.Threshold = Float'Last or
                               Best.Threshold = (-Float'Last) then
                              Best.Threshold :=
                                Float (Features_X.Element (P_Index - 1).
                                           Integer_Value);
                           end if;

                        when Boolean_Type | UB_String_Type => null;
                     end case;
                     --  L420
                  end if;
               end if;
            end if;
         end if;
         P1_Index := P_Index + 1;
      end loop;

   end Find_Best_Split;

   --  -------------------------------------------------------------------------

   procedure Process_Constants
     (Self                : in out Splitter_Class;
      Features            : in out Classifier_Types.Natural_List;
      Current             : in out Split_Record;
      Num_Features, Num_Known_Constants,
      Max_Features        : Natural;
      Num_Visited_Features, Num_Drawn_Constants, Num_Found_Constants,
      Num_Total_Constants : in out Natural;
      Best_Split          : in out Split_Record) is
      use ML_Types;
      Samples_I     : Positive;
      X_Samples     : Natural;
      X_Features    : Value_Data_List;
      F_I           : Natural := Num_Features;
      F_J           : Natural;
      Compare_Value : Value_Record;
      Swap          : Natural;
   begin
      --  L323
      Put_Line ("Node_Splitter.Process_Constants F_I, Num_Total_Constants: "
                & Integer'Image (F_I) & ", " &
                  Integer'Image (Num_Total_Constants));

      while F_I > Num_Total_Constants and
        (Num_Visited_Features < Max_Features or
         --   At least one drawn feature must be non constant.
           Num_Visited_Features <=
             Num_Found_Constants + Num_Drawn_Constants) loop
         --  L329
         Num_Visited_Features := Num_Visited_Features + 1;
         --  L342
         --  Draw a feature at random;
         F_J := Num_Drawn_Constants + 1 +
           Maths.Random_Integer mod (F_I - Num_Found_Constants);
         Put_Line ("Node_Splitter.Process_Constants, Num_Known_Constants: "
                   & Integer'Image (Num_Known_Constants));
         Put_Line ("Node_Splitter.Process_Constants, Num_Drawn_Constants: "
                   & Integer'Image (Num_Drawn_Constants));

         if F_J < Num_Known_Constants then
            Swap := Num_Drawn_Constants;
            Put_Line ("Node_Splitter.Process_Constants Swap: "
                      & Integer'Image (Swap));
            Features.Replace_Element
              (Num_Drawn_Constants, Features.Element (F_J));
            Put_Line ("Node_Splitter.Process_Constants Num_Drawn_Constants: "
                      & Integer'Image (Num_Drawn_Constants));
            Features.Replace_Element (F_J, Features.Element (Swap));
            Num_Drawn_Constants := Num_Drawn_Constants + 1;
            --  L356
         else
            F_J := F_J + Num_Found_Constants;
            Put_Line ("Node_Splitter.Process_Constants else F_J: " &
                        Integer'Image (F_J));
            if F_J = 0 then
               F_J := 1;
            end if;

            Current.Feature := Features.Element (F_J);
            Put_Line ("Node_Splitter.Process_Constants Feature: " &
                        Integer'Image (Current.Feature));

            --  L364
            for index in Self.Start_Index .. Self.End_Index loop
               Put_Line ("Node_Splitter.Process_Constants index: "
                         & Integer'Image (index));
               Samples_I := Self.Sample_Indices.Element (F_I);
               Put_Line ("Node_Splitter.Process_Constants Samples_I: "
                         & Integer'Image (Samples_I));
               X_Samples := Self.Sample_Indices.Element (Samples_I);
               --  X_Features is a Value_Data_List
               X_Features := Self.X.Element (X_Samples);
               Self.Feature_Values.Replace_Element
                 (Features (index), X_Features (Current.Feature));
            end loop;

            --  L367
            Replacement_Sort (X_Features,
                              Self.Start_Index, Self.End_Index);

            --  L369  Feature_Values is a value_data_list
            if Self.Feature_Values.First_Element.Value_Kind = Float_Type then
               Compare_Value.Float_Value :=
                 Self.Feature_Values.First_Element.Float_Value +
                   Feature_Threshold;
            else
               Compare_Value := Self.Feature_Values.First_Element;
            end if;

            --  Still L369
            if Self.Feature_Values.Element (Self.Feature_Values.Last_Index - 1) <=
              Compare_Value then
               Swap := Features.Element (F_J);
               Features.Replace_Element
                 (F_J, Features.Element (Num_Total_Constants));
               Features.Replace_Element (Num_Total_Constants, Swap);

               Num_Found_Constants := Num_Found_Constants + 1;
               Num_Total_Constants := Num_Total_Constants + 1;

            else  --  L374
               F_I := F_I - 1;
               Swap := Features.Element (F_I);
               Features.Replace_Element (F_I, Features.Element (F_J));
               Features.Replace_Element (F_J, Swap);

               --  Evaluate all splits
               Criterion.Reset (Self.Criteria);
               --  L381
               Find_Best_Split (Self, Features, Self.Feature_Values,
                                Current, Best_Split);
            end if;
         end if;
      end loop;

   end Process_Constants;

   --  -------------------------------------------------------------------------

   procedure Reorganize_Samples (Self        : in out Splitter_Class;
                                 Best_Split  : in out Split_Record;
                                 X_Samples   : in out
                                   ML_Types.List_Of_Value_Data_Lists;
                                 Impurity    : Float) is
      Partition_End : Natural;
      P_Index       : Positive;
      X_1           : ML_Types.Value_Data_List;
      Swap          : ML_Types.Value_Data_List;
      Crit          : Criterion.Criterion_Class;
   begin
      --  Reorganize into samples[start:best.pos] + samples[best.pos:end]
      if Best_Split.Pos_I < Self.End_Index then
         Partition_End := Self.End_Index;
         P_Index := Self.Start_Index;
         while P_Index < Partition_End loop
            X_1 := Self.X.Element (Self.Sample_Indices.Element (P_Index));
            if X_1.Element
              (Self.Sample_Indices.Element (P_Index)).Float_Value <=
                Best_Split.Threshold then
               P_Index := P_Index + 1;
            else
               Partition_End := Partition_End - 1;
               Swap := X_Samples.Element (P_Index);
               X_Samples.Replace_Element
                 (P_Index, X_Samples.Element (Partition_End));
               X_Samples.Replace_Element (Partition_End, Swap);
            end if;
         end loop;

         --  L436
         Criterion.Reset (Self.Criteria);
         Crit := Self.Criteria;
         Crit.Position := Best_Split.Pos_I;
         Criterion.Update (Self.Criteria, Crit.Position);

         Criterion.Gini_Children_Impurity
           (Self.Criteria, Best_Split.Impurity_Left, Best_Split.Impurity_Right);
         Best_Split.Improvement := Criterion.Impurity_Improvement
           (Self.Criteria, Impurity, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
      end if;

   end Reorganize_Samples;

   --  -------------------------------------------------------------------------

   procedure Replacement_Sort (Data        : in out ML_Types.Value_Data_List;
                               Start, Stop : Positive) is
      use ML_Types;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Temp : Value_Data_List;
   begin
      for index in Start .. Stop loop
         Temp.Append (Data.Element (index));
      end loop;

      Sort (Temp);
      for index in Start .. Stop loop
         Data.Replace_Element (index, Temp.Element (index));
      end loop;

   end Replacement_Sort;

   --  -------------------------------------------------------------------------
   --  Reset_Node resets the splitter Split based on node Split.Samples[start:end].
   procedure Reset_Node
     (Splitter              : in out Splitter_Class;
      Start, Stop           : Positive;
      Classes               : ML_Types.List_Of_Value_Data_Lists;
      Weighted_Node_Samples : in out Float) is
   begin
      Criterion.Init (Splitter.Criteria, Classes);
      Splitter.Start_Index := Start;
      Splitter.End_Index := Stop;
      Criterion.Classification_Init
        (Splitter.Criteria, Splitter.Y, Splitter.Sample_Indices,
         Splitter.Sample_Weight, Splitter.Weighted_Samples, Start, Stop);

      Weighted_Node_Samples := Splitter.Criteria.Weighted_Node_Samples;

   end Reset_Node;

   --  -------------------------------------------------------------------------
   --  BestSplitter.Split_Node samples up to max_features without replacement
   --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
   --  and `f_j` to compute a permutation of the `features` array).
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record is
      Num_Features              : constant Natural :=
                                    Natural (Self.Feature_Indices.Length);
      Max_Features              : constant Natural := Self.Max_Features;
      X_Samples                 : ML_Types.List_Of_Value_Data_Lists := Self.X;

      Features                  : Classifier_Types.Natural_List :=
                                    Self.Feature_Indices;
      Current_Split             : Split_Record;
      Num_Known_Constants       : constant Natural := Num_Constant_Features;
      Num_Total_Constants       : Natural := Num_Known_Constants;
      Num_Visited_Features      : Natural := 0;
      Num_Found_Constants       : Natural := 0;
      Num_Drawn_Constants       : Natural := 0;
      Best_Split                : Split_Record;
   begin
      if Integer (X_Samples.Length) = 0 then
         raise Node_Splitter_Error with
           "Node_Splitter.Split_Node called with empty Self.X";
      end if;
      Init_Split (Best_Split);

      --  L323
      Process_Constants
        (Self, Features, Current_Split, Num_Features, Num_Known_Constants,
         Max_Features, Num_Visited_Features, Num_Drawn_Constants,
         Num_Found_Constants, Num_Total_Constants, Best_Split);

      Put_Line ("Node_Splitter.Split_Node Process_Constants done.");
      --  L421
      Reorganize_Samples (Self, Best_Split, X_Samples, Impurity);

      --  L448
      --  Respect invariant for constant features: the original order of
      --  element in features[:n_known_constants] must be preserved for
      --  sibling and child nodes.
      for index in 1 .. Num_Known_Constants loop
         Features.Replace_Element
           (index, Self.Constant_Features_I.Element (index));
      end loop;

      --   Copy newly found constant features
      for index in Num_Known_Constants + 1 .. Num_Found_Constants loop
         Self.Constant_Features_I.Replace_Element
           (index, Features.Element (index));
      end loop;

      Num_Constant_Features := Num_Total_Constants;
      return Best_Split;

   end Split_Node;

end Node_Splitter;
