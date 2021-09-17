--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Containers;

with Maths;

package body Node_Splitter is

   Feature_Threshold : constant Float := 10.0 ** (-7);

   procedure Replacement_Sort (Self        : Splitter_Class;
                               Data        : in out ML_Types.Value_Data_List;
                               Current     : Split_Record);

   procedure Replacement_Sort
     (Self      : Splitter_Class;
      X_Samples : in out ML_Types.Value_Data_List;
      Y_Samples : in out ML_Types.Value_Data_List;
      Current   : Split_Record);

   --  -------------------------------------------------------------------------

   procedure Init (Self          : in out Splitter_Class;
                   X, Y          : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight : Classifier_Types.Weight_List) is
      use Ada.Containers;
      Num_Samples      : constant Positive := Positive (X.Element (1).Length);
      Num_Features     : constant Positive := Positive (X.Length);
      Samples          : Classifier_Types.Natural_List;
      Weighted_Samples : Float := 0.0;
      J                : Natural := 0;
   begin
      for index in 1 .. Num_Samples loop
         if not Sample_Weight.Is_Empty then
            if Sample_Weight.Element (index)  > 0.0 then
               Samples.Append (index);
               J := J + 1;
            end if;
            Weighted_Samples :=
              Weighted_Samples + Sample_Weight.Element (index);
         else
            Weighted_Samples := Weighted_Samples + 1.0;
         end if;
      end loop;

      Self.Num_Samples := J;
      Self.Weighted_Samples := Weighted_Samples;

      Self.Feature_Indices.Clear;
      for index in 1 .. Num_Features loop
         Self.Feature_Indices.Append (index);
      end loop;
      Self.Feature_Values.Clear;
      Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
      Self.Constant_Features.Clear;
      Self.Constant_Features.Set_Length (Count_Type (Num_Features));

      Self.Y := Y;
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

   procedure Process_A (Self        : in out Splitter_Class;
                        --                          P           : Natural;
                        Features    : Classifier_Types.Natural_List;
                        Features_X  : ML_Types.Value_Data_List;
                        Cur         : Split_Record;
                        Best        : in out Split_Record) is
      use ML_Types;
      Criteria                  : Criterion.Criterion_Class;
      P_Index                   : Natural := Features.First_Index;
      P1_Index                  : Natural;
      Current                   : Split_Record := Cur;
      Current_Proxy_Improvement : Float := -Float'Last;
      Best_Proxy_Improvement    : Float := -Float'Last;
   begin
      --  L380 Evaluate all splits
      Criterion.Reset (Self.Criteria);
      Criteria := Self.Criteria;

      while P_Index <= Features.Last_Index loop
         P1_Index := P_Index + 1;
         while P1_Index <= Features.Last_Index loop
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
         end loop;

         P_Index := P_Index + 1;
         --  L395
         if P_Index <= Features.Last_Index then
            Current.Pos := P_Index;
            --  L398 Reject if min_samples_leaf is not guaranteed
            if Self.Criteria.Position >= Self.Min_Leaf_Samples and
              Features.Last_Index >= Self.Min_Leaf_Samples then
               Criteria.Position := Current.Pos;
               Criterion.Update (Self.Criteria, Criteria);

               --  L405 Reject if min_weight_leaf is not satisfied
               if Self.Criteria.Weighted_Left >= Self.Min_Leaf_Weight and
                 Self.Criteria.Weighted_Right >= Self.Min_Leaf_Weight then
                  Current_Proxy_Improvement := Self.Criteria.Proxy_Improvement;
                  if Current_Proxy_Improvement > Best_Proxy_Improvement then
                     Best_Proxy_Improvement := Current_Proxy_Improvement;
                     --  L415
                     case Features_X.Element (P1_Index).Value_Kind is

                     when Float_Type =>
                        Current.Threshold :=
                          0.5 * (Features_X.Element (P_Index - 1).Float_Value +
                                     Features_X.Element (P_Index).Float_Value);
                        if Current.Threshold =
                          Features_X.Element (P_Index).Float_Value or
                          Current.Threshold = Float'Last or
                          Current.Threshold = (-Float'Last) then
                           Current.Threshold :=
                             Features_X.Element (P_Index - 1).Float_Value;
                        end if;

                     when Integer_Type =>
                        Current.Threshold :=
                          0.5 * Float (Features_X.Element (P_Index - 1).Integer_Value +
                                           Features_X.Element (P_Index).Integer_Value);
                        if Current.Threshold =
                          Float (Features_X.Element (P_Index).Integer_Value) or
                          Current.Threshold = Float'Last or
                          Current.Threshold = (-Float'Last) then
                           Current.Threshold :=
                             Float
                               (Features_X.Element (P_Index - 1).Integer_Value);
                        end if;
                     when Boolean_Type | UB_String_Type => null;
                     end case;
                     --  L422
                     Best := Current;
                  end if;
               end if;
            end if;
         end if;
      end loop;

   end Process_A;

   --  -------------------------------------------------------------------------

   procedure Process_B (Self        : in out Splitter_Class;
                        Best_Split  : in out Split_Record;
                        X_Samples   : ML_Types.List_Of_Value_Data_Lists;
                        Y_Samples   : ML_Types.List_Of_Value_Data_Lists;
                        Impurity    : Float) is
      Partition_End : Natural;
      P_Index       : Positive;
      X_1           : ML_Types.Value_Data_List;
      Swap          : Natural;
   begin
      --  L424 Reorganize into samples[start:best.pos] + samples[best.pos:end]
      if Best_Split.Pos < X_Samples.Last_Index then
         Partition_End := X_Samples.Last_Index;
         P_Index := X_Samples.First_Index;
         while P_Index < Partition_End loop
            X_1 := Self.X.Element (P_Index);
            if X_1.Element (Best_Split.Feature).Float_Value
              <= Best_Split.Threshold then
               P_Index := P_Index + 1;
            else
               Partition_End := Partition_End - 1;
               Swap := Samples.Element (P_Index);
               Samples.Replace_Element
                 (P_Index, Samples.Element (Partition_End));
               Samples.Replace_Element (Partition_End, Swap);
            end if;
         end loop;

         Criterion.Reset (Self.Criteria);
         --        Criterion.Update (Self.Criteria);
         Criterion.Children_Impurity
           (Self.Criteria, Best_Split.Impurity_Left, Best_Split.Impurity_Right);
         Best_Split.Improvement := Criterion.Impurity_Improvement
           (Self.Criteria, Impurity, Best_Split.Impurity_Left,
            Best_Split.Impurity_Right);
      end if;

   end Process_B;

   --  -------------------------------------------------------------------------

   procedure Replacement_Sort (Self    : Splitter_Class;
                               Data    : in out ML_Types.Value_Data_List;
                               Current : Split_Record) is
      use ML_Types;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Temp : Value_Data_List;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         Temp.Append (Data.Element (index));
      end loop;

      Sort (Temp);
      for index in Data.First_Index .. Data.Last_Index loop
         null;
         --           Data.Replace_Element
         --             (Data.To_Cursor (Self.Feature_Values.Element (index)),
         --              Temp.Element (Current.Feature_Index));
      end loop;

   end Replacement_Sort;

   --  -------------------------------------------------------------------------

   procedure Replacement_Sort
     (Self      : Splitter_Class;
      X_Samples : in out ML_Types.Value_Data_List;
      Y_Samples : in out ML_Types.Value_Data_List;
      Current   : Split_Record) is
      use ML_Types;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Temp_X : Value_Data_List;
      Temp_Y : Value_Data_List;
   begin
      for index in X_Samples.First_Index .. X_Samples.Last_Index loop
         Temp_X.Append (X_Samples.Element (index));
         Temp_Y.Append (Y_Samples.Element (index));
      end loop;

      Sort (Temp_X);
      Sort (Temp_Y);
      --        for index in X_Samples.First_Index .. X_Samples.Last_Index loop
      --           X_Samples.Replace_Element
      --             (X_Samples.To_Cursor (Self.X_Samples.Element (index)),
      --              Temp_X.Element (Current.Feature_Index));
      --        end loop;

   end Replacement_Sort;

   --  -------------------------------------------------------------------------

   procedure Reset_Node
     (Split                 : in out Splitter_Class;
      Weighted_Node_Samples : in out Float) is
   begin
      Criterion.Init (Split.Criteria, Split.Y, Split.Sample_Weight,
                      Split.Weighted_Samples, Split.X_Samples, Split.Y_Samples);

      Weighted_Node_Samples := Split.Criteria.Weighted_Node_Samples;

   end Reset_Node;

   --  -------------------------------------------------------------------------
   --  BestSplitter.Split_Node samples up to max_features without replacement
   --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
   --  and `f_j` to compute a permutation of the `features` array).
   function Split_Node (Self              : in out Splitter_Class;
                        Impurity          : Float;
                        Constant_Features : in out ML_Types.Value_Data_List)
                        return Split_Record is
      use ML_Types;
      use ML_Types.Value_Data_Package;
      Num_Features              : constant Natural :=
                                    Natural (Self.Feature_Indices.Length);
      Max_Features              : constant Natural := Self.Max_Features;
      Known_Constants           : constant ML_Types.Value_Data_List :=
                                    Constant_Features;
      X_Samples                 : ML_Types.List_Of_Value_Data_Lists :=
                                    Self.X_Samples;
      Y_Samples                 : ML_Types.List_Of_Value_Data_Lists :=
                                    Self.Y_Samples;

      X_Sample                  : ML_Types.Value_Data_List;
      Y_Sample                  : ML_Types.Value_Data_List;
      Features                  : Classifier_Types.Natural_List :=
                                    Self.Feature_Indices;
      Features_X                : Value_Data_List := Self.Feature_Values;
      Current_Split             : Split_Record;
      Num_Known_Constants       : constant Natural := Natural (Known_Constants.Length);
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
            --  L351
         else
            F_J := F_J + Num_Found_Constants;
            --              Current_Split.Feature_Index := Features.Element (F_J);
            Features_X.Clear;
            for index in Features.First_Index .. Features.Last_Index loop
               null;
               --                 Features_X.Append
               --                   (Self.Feature_Values.Element (Current_Split.Feature_Index));
            end loop;
            --  L364
            Replacement_Sort (Self, Features_X, Current_Split);
            for index in X_Samples.First_Index .. X_Samples.Last_Index loop
               X_Sample := X_Samples.Element (index);
               Y_Sample := Y_Samples.Element (index);
               Replacement_Sort (Self, X_Sample, Y_Sample, Current_Split);
               X_Samples.Replace_Element (index, X_Sample);
               Y_Samples.Replace_Element (index, Y_Sample);
            end loop;

            --  L367  Features_X is a value_data_list
            if Features_X.First_Element.Value_Kind = Float_Type then
               Compare_Value.Float_Value :=
                 Features_X.First_Element.Float_Value +
                   Feature_Threshold;
            else
               Compare_Value := Features_X.First_Element;
            end if;
            --  Still L367
            if Features_X.Last_Element <= Compare_Value then
               Swap := Features.Element (F_J);
               Features.Replace_Element
                 (F_J, Features.Element (Num_Total_Constants));
               Features.Replace_Element (Num_Total_Constants, Swap);

               Num_Found_Constants := Num_Found_Constants + 1;
               Num_Total_Constants := Num_Total_Constants + 1;
               --  L372
            else
               F_I := F_I - 1;
               Swap := Features.Element (F_I);
               Features.Replace_Element (F_I, Features.Element (F_J));
               Features.Replace_Element (F_J, Swap);

               --  Evaluate all splits
               Criterion.Reset (Self.Criteria);
               --  L381
               Process_A (Self, Features, Features_X, Current_Split, Best_Split);
            end if;
         end if;
      end loop;

      --  L421
      Process_B (Self, Best_Split, X_Samples, Y_Samples, Impurity);

      --  L443
      --  Respect invariant for constant features: the original order of
      --  element in features[:n_known_constants] must be preserved for
      --  sibling and child nodes.
      for index in 1 .. Num_Known_Constants loop
         Features.Replace_Element
           (index, Self.Constant_Features.Element (index));
      end loop;

      --   Copy newly found constant features
      for index in Num_Known_Constants + 1 .. Num_Found_Constants loop
         Self.Constant_Features.Replace_Element
           (index, Features.Element (index));
      end loop;

      Constant_Features.Replace_Element
        (1, Constant_Features.Element (Num_Total_Constants));
      return Best_Split;

   end Split_Node;

end Node_Splitter;
