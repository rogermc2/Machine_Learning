--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Node_Splitter is

   Feature_Threshold : constant Float := 10.0 ** (-7);

   procedure Replacement_Sort (X           : in out ML_Types.Value_Data_List;
                               Y           : in out ML_Types.Value_Data_List;
                               Start, Stop : Positive);

   --  -------------------------------------------------------------------------

   procedure Init (Self              : in out Splitter_Class;
                   Input_X, Target_Y : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight     : Classifier_Types.Weight_List) is
      use Ada.Containers;
      Num_Samples      : constant Positive := Positive (Input_X.Element (1).Length);
      Num_Features     : constant Positive := Positive (Input_X.Length);
      Samples          : Classifier_Types.Natural_List;
      Weighted_Samples : Float := 0.0;
      J                : Positive := 1;
   begin
      Samples.Set_Length (Count_Type (Num_Samples));
      for index in 1 .. Num_Samples loop
         --  Only work with positively weighted samples.
         if Sample_Weight.Is_Empty or else
           Sample_Weight.Element (index) > 0.0 then
            Samples.Replace_Element (J, index);
            J := J + 1;
         end if;

         if Sample_Weight.Is_Empty then
            Weighted_Samples := Weighted_Samples + 1.0;
         else
            Weighted_Samples :=
              Weighted_Samples + Sample_Weight.Element (index);
         end if;
      end loop;

      --  Number of samples is the number of positively weighted samples.
      Self.Num_Samples := J;
      Self.Min_Leaf_Samples := Num_Samples;
      Self.Weighted_Samples := Weighted_Samples;
      Put_Line ("Node_Splitter.Init Self.Num_Samples, Weighted_Samples: " &
                  Integer'Image (Self.Num_Samples) & ", " &
                  Float'Image (Weighted_Samples));

      Self.Feature_Indices.Clear;
      for index in 1 .. Num_Features loop
         Self.Feature_Indices.Append (index);
      end loop;
      Self.Feature_Values.Clear;
      Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
      Self.Constant_Features_I.Clear;
      Self.Constant_Features_I.Set_Length (Count_Type (Num_Features));

      Self.Target_Y := Target_Y;
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

   procedure Node_Value (Self : Splitter_Class; Value : out Float) is
   begin
      Criterion.Node_Value (Self.Criteria, Value);
   end Node_Value;

   --  -------------------------------------------------------------------------

   procedure Process_A (Self       : in out Splitter_Class;
                        Features   : Classifier_Types.Natural_List;
                        Features_X : ML_Types.Value_Data_List;
                        Current    : Split_Record;
                        Best       : in out Split_Record) is
      use ML_Types;
      Criteria                  : Criterion.Criterion_Class;
      P_Index                   : Natural := Features.First_Index;
      P1_Index                  : Natural;
      Current_Proxy_Improvement : Float := -Float'Last;
      Best_Proxy_Improvement    : Float := -Float'Last;
   begin
      --  L379 Evaluate all splits
      Best := Current;
      Criterion.Reset (Self.Criteria);
      Criteria := Self.Criteria;
      P1_Index := Self.Start_Index;

      while P_Index <= Self.End_Index loop
         P1_Index := P_Index + 1;
         while P1_Index <= Self.End_Index loop
            case Features_X.Element (P1_Index).Value_Kind is
               when Boolean_Type =>
                  if not Features_X.Element (P_Index).Boolean_Value and
                    Features_X.Element (P1_Index).Boolean_Value then
                     P_Index := P_Index + 1;
                  end if;
               when Float_Type =>
                  if Features_X.Element (P1_Index).Float_Value <=
                    Features_X.Element (P_Index).Float_Value + Feature_Threshold then
                     P_Index := P_Index + 1;
                  end if;
               when Integer_Type =>
                  if Features_X.Element (P1_Index).Integer_Value <=
                    Features_X.Element (P_Index).Integer_Value then
                     P_Index := P_Index + 1;
                  end if;
               when UB_String_Type =>
                  if Features_X.Element (P1_Index).UB_String_Value <=
                    Features_X.Element (P_Index).UB_String_Value then
                     P_Index := P_Index + 1;
                  end if;
            end case;
         end loop; --  P1_Index

         P_Index := P_Index + 1;
         --  L393
         if P_Index <= Self.End_Index then
            Best.Pos_I := P_Index;
            --  L398 Reject if min_samples_leaf is not guaranteed
            if Current.Pos_I - Self.Start_Index >= Self.Min_Leaf_Samples and
              Self.End_Index - Current.Pos_I >= Self.Min_Leaf_Samples then
               --  L401
               Criteria.Position := Best.Pos_I;
               Criterion.Update (Self.Criteria, Criteria);

               --  L405 Reject if min_weight_leaf is not satisfied
               if Self.Criteria.Weighted_Left >= Self.Min_Leaf_Weight and
                 Self.Criteria.Weighted_Right >= Self.Min_Leaf_Weight then
                  Current_Proxy_Improvement :=
                    Self.Criteria.Proxy_Improvement;

                  if Current_Proxy_Improvement >
                    Best_Proxy_Improvement then
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
                              Best.Threshold := Features_X.Element
                                (P_Index - 1).Float_Value;
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
      end loop;

   end Process_A;

   --  -------------------------------------------------------------------------

   procedure Process_B (Self        : in out Splitter_Class;
                        Best_Split  : in out Split_Record;
                        X_Samples   : in out ML_Types.List_Of_Value_Data_Lists;
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
            X_1 := Self.Input_X.Element
              (Self.Sample_Indices.Element (P_Index));
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
         Criterion.Update (Self.Criteria, Crit);

         Criterion.Children_Impurity
           (Self.Criteria, Best_Split.Impurity_Left, Best_Split.Impurity_Right);
         Best_Split.Improvement := Criterion.Impurity_Improvement
           (Self.Criteria, Impurity, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
      end if;

   end Process_B;

   --  -------------------------------------------------------------------------

   procedure Replacement_Sort (X           : in out ML_Types.Value_Data_List;
                               Y           : in out ML_Types.Value_Data_List;
                               Start, Stop : Positive) is
      use ML_Types;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Temp_X : Value_Data_List;
      Temp_Y : Value_Data_List;
   begin
      for index in Start .. Stop loop
         Temp_X.Append (X.Element (index));
         Temp_Y.Append (Y.Element (index));
      end loop;

      Sort (Temp_X);
      Sort (Temp_Y);
      for index in Start .. Stop loop
         X.Replace_Element (index, Temp_X.Element (index));
         Y.Replace_Element (index, Temp_Y.Element (index));
      end loop;

   end Replacement_Sort;

   --  -------------------------------------------------------------------------

   procedure Reset_Node
     (Split                 : in out Splitter_Class;
      Weighted_Node_Samples : in out Float) is
   begin
      Criterion.Init
        (Split.Criteria, Split.Target_Y, Split.Sample_Weight,
         Split.Weighted_Samples, Split.X_Samples, Split.Y_Samples);

      Weighted_Node_Samples := Split.Criteria.Weighted_Node_Samples;

   end Reset_Node;

   --  -------------------------------------------------------------------------
   --  BestSplitter.Split_Node samples up to max_features without replacement
   --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
   --  and `f_j` to compute a permutation of the `features` array).
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                         return Split_Record is
      use ML_Types;
      use ML_Types.Value_Data_Package;
      Num_Features              : constant Natural :=
                                    Natural (Self.Feature_Indices.Length);
      Max_Features              : constant Natural := Self.Max_Features;
      X_Samples                 : ML_Types.List_Of_Value_Data_Lists :=
                                    Self.X_Samples;

      X_Sample                  : ML_Types.Value_Data_List;
      Y_Sample                  : ML_Types.Value_Data_List;
      Features                  : Classifier_Types.Natural_List :=
                                    Self.Feature_Indices;
      Features_X                : Value_Data_List := Self.Feature_Values;
      Current_Split             : Split_Record;
      Num_Known_Constants       : constant Natural := Num_Constant_Features;
      Num_Total_Constants       : Natural := Num_Known_Constants;
      Num_Visited_Features      : Natural := 0;
      Num_Found_Constants       : Natural := 0;
      Num_Drawn_Constants       : Natural := 0;
      F_I                       : Natural := Num_Features;
      F_J                       : Natural;
      Swap                      : Natural;
      Compare_Value             : Value_Record;
      Best_Split                : Split_Record;
   begin
      --  Skip CPU intensive evaluation of the impurity criterion for
      --  features that have already been detected as constant
      --  (hence not suitable for good splitting) by ancestor nodes and save
      --  the information on newly discovered constant features to avoid
      --  computation on descendant nodes.
      Init_Split (Best_Split);

      --  L323
      while F_I > Num_Total_Constants and
        (Num_Visited_Features < Max_Features or
         --   At least one drawn features must be non constant.
           Num_Visited_Features <= Num_Found_Constants + Num_Drawn_Constants)
      loop
         --  L329
         Num_Visited_Features := Num_Visited_Features + 1;
         --  L342
         --  Draw a feature at random;
         F_J := Num_Drawn_Constants +
           Maths.Random_Integer mod (F_I - Num_Found_Constants);

         if F_J < Num_Known_Constants then
            Swap := Num_Drawn_Constants;
            Features.Replace_Element
              (Num_Drawn_Constants, Features.Element (F_J));
            Features.Replace_Element (F_J, Features.Element (Swap));
            Num_Drawn_Constants := Num_Drawn_Constants + 1;
            --  L356
         else
            F_J := F_J + Num_Found_Constants;
            Current_Split.Feature_Index := Features.Element (F_J);

            --  L364
            for index in X_Samples.First_Index .. X_Samples.Last_Index loop
               X_Sample := Self.X_Samples.Element (F_I);
               Features_X.Replace_Element
                 (Index, X_Sample.Element (Current_Split.Feature_Index));
            end loop;

            --  L367
            Replacement_Sort (X_Sample, Y_Sample,
                              Self.Start_Index, Self.End_Index);

            --  L369  Features_X is a value_data_list
            if Features_X.First_Element.Value_Kind = Float_Type then
               Compare_Value.Float_Value :=
                 Features_X.First_Element.Float_Value +
                   Feature_Threshold;
            else
               Compare_Value := Features_X.First_Element;
            end if;

            --  Still L369
            if Features_X.Element (Features_X.Last_Index - 1) <=
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
               Process_A (Self, Features, Features_X, Current_Split,
                          Best_Split);
            end if;
         end if;
      end loop;

      --  L421
      Process_B (Self, Best_Split, X_Samples, Impurity);

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
