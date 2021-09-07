
with Ada.Containers;

with ML_Types;

package body Criterion is

   --  ------------------------------------------------------------------------
   --  Node_Impurity evaluates the Gini criterion as the impurity of the
   --  current node
   function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                return Float is
      use Ada.Containers;
--        use Classifier_Types.Weight_List_Package;
      Sum_Total : Classifier_Types.Weight_List;
      Count_K   : Float;
      Gini      : Float := 0.0;
      Sq_Count  : Float := 0.0;
   begin
--        Sum_Total := Criteria.Sum_Total.Element (1);
      for index_k in Positive range 1 .. Criteria.Num_Outputs loop
         Sq_Count := 0.0;
--           Sum_Total := Criteria.Sum_Total.Element (index_k);
         if Criteria.Classes.Length > Sum_Total.Length then
            Sum_Total.Set_Length (Criteria.Classes.Length);
--           if Natural (Criteria.Classes.Length) > Criteria.Stride then
--              Criteria.Stride  := Natural (Criteria.Classes.Length);
         end if;

         for Class_Index in Criteria.Classes.First_Index ..
           Criteria.Classes.Last_Index loop
            Count_K := Float (Sum_Total.Element (Class_Index));
            Sq_Count := Sq_Count + Count_K ** 2;
         end loop;

         Gini := Gini +
           1.0 - Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
      end loop;

      return Gini / Float (Criteria.Num_Outputs);

   end Gini_Node_Impurity;

   --  ------------------------------------------------------------------------

   procedure Init (Criteria         : in out Criterion_Class;
                   Y                : ML_Types.List_Of_Value_Data_Lists;
                   --  Sample_Weight contains the weight of each sample
                   Sample_Weight    : Classifier_Types.Weight_List;
                   Weighted_Samples : Float; Start, Stop : Natural;
                   Sample_Indices   : Classifier_Types.Natural_List) is
      Y_I       : ML_Types.Value_Data_List;
      Sum_Total : Classifier_Types.Weight_List;
      i         : Natural;
      Weight    : float := 1.0;
      Y_Ik      : Float;
      W_Ik      : Float;
   begin
      Criteria.Y := Y;
      Criteria.Sample_Weight := Sample_Weight;
      Criteria.Sample_Indices := Sample_Indices;
      Criteria.Start := Start;
      Criteria.Stop := Stop;
      Criteria.Num_Node_Samples := Stop - Start;
      Criteria.Weighted_Samples := Weighted_Samples;
      Criteria.Num_Weighted_Node_Samples := 0;

      Criteria.Sq_Sum_Total := 0.0;

      Criteria.Sum_Total.Clear;
      for k in 1 .. Criteria.Num_Outputs loop
         Sum_Total.Append (0.0);
         for index in 1 .. Criteria.Classes.Length loop
            Sum_Total.Append (0.0);
         end loop;
         Criteria.Sum_Total.Append (Sum_Total);
      end loop;

      for p in Start .. Stop loop
         i := Criteria.Sample_Indices.Element (p);
         Y_I := Y.Element (i);

         --  Weight is originally set to be 1.0, meaning that if no
         --  sample weights are given, the default weight of each sample is 1.0
         if not Sample_Weight.Is_Empty then
            Weight := Sample_Weight.Element (i);
         end if;

         for k in 1 .. Criteria.Num_Outputs loop
            Y_Ik := Float (Y_I.Element (k).Integer_Value);
            W_Ik := Y_Ik * Weight;
            Sum_Total.Replace_Element
              (k, Sum_Total.Element (k) + W_Ik);
            Criteria.Sq_Sum_Total := Criteria.Sq_Sum_Total + Y_Ik * W_Ik;
         end loop;

         Criteria.Weighted_Node_Samples.Replace_Element (p,
           Criteria.Weighted_Node_Samples.Element (p) + Weight);
      end loop;

      Reset (Criteria);

   end Init;

   --  ------------------------------------------------------------------------

   procedure Reset (Criteria : in out Criterion_Class) is
      Sum_Left         : Classifier_Types.Weight_List;
      Sum_Right        : Classifier_Types.Weight_List;
      Sum_Total        : Classifier_Types.Weight_List;
   begin
      Criteria.Sum_Right.Clear;
      Criteria.Sum_Left.Clear;
      for k in 1 .. Criteria.Num_Outputs loop
         Criteria.Sum_Right.Append (0.0);
         Criteria.Sum_Left.Append (Criteria.Sum_Total.Element (k));
      end loop;

         Criteria.Sum_Left.Replace_Element (k, Sum_Left);
         Criteria.Sum_Right.Replace_Element (k, Sum_Right);

   end Reset;

   --  ------------------------------------------------------------------------

   procedure Reverse_Reset (Criteria : in out Criterion_Class) is
   begin
      null;
   end Reverse_Reset;

   --  ------------------------------------------------------------------------
   --  Update statistics by moving samples[pos:new_pos] to the left child.
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive) is
      use ML_Types;
      Sum_Left    : Classifier_Types.Weight_List :=
                        Criteria.Sum_Left.Element (1);
      Sum_Right   : Classifier_Types.Weight_List;
      Sum_Total   : Classifier_Types.Weight_List;
      i           : Positive;
      Label_Index : Positive;
      Values      : Value_Data_List;
      Weight      : Float := 1.0;
   begin
      if (New_Pos - Criteria.Pos) <= (Criteria.Stop - New_Pos) then
         for p in Criteria.Pos .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Values := Criteria.Y.Element (i);
            for k in 1 .. Criteria.Num_Outputs loop
               Sum_Left := Criteria.Sum_Left.Element (k);
               Label_Index := Values.Element (k).Integer_Value;
               Sum_Left.Replace_Element
                 (Label_Index, Sum_Left.Element (Label_Index) + Weight);
               Criteria.Sum_Left.Replace_Element (k, Sum_Left);
            end loop;

            Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left + Weight;
         end loop;

      else
         Reverse_Reset (Criteria);
         for p in reverse Criteria.Stop .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Values := Criteria.Y.Element (i);
            for k in 1 .. Criteria.Num_Outputs loop
               Sum_Left := Criteria.Sum_Left.Element (k);
               Label_Index := Values.Element (k).Integer_Value;
               Sum_Left.Replace_Element
                 (Label_Index, Sum_Left.Element (Label_Index) - Weight);
               Criteria.Sum_Left.Replace_Element (k, Sum_Left);
            end loop;

            Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left - Weight;
         end loop;
      end if;

      --  Update right part statistics
      Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples -
          Criteria.Num_Weighted_Left;
      for k in 1 .. Criteria.Num_Outputs loop
            for c in 1 .. Criteria.Num_Classes.Element (k) loop
               Sum_Right.Replace_Element
                 (c, Sum_Total.Element (c) - Sum_Left.Element (c));
            end loop;

      end loop;

   end Update;

   --  ------------------------------------------------------------------------

end Criterion;
