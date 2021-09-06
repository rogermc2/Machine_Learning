
with ML_Types;

package body Criterion is

   --  ------------------------------------------------------------------------
   --  Node_Impurity evaluates the Gini criterion as the impurity of the
   --  current node
   function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                return Float is
      use Classifier_Types.Natural_Package;
      Sum_Total : Classifier_Types.Natural_List;
      Count_K   : Float;
      Gini      : Float := 0.0;
      Sq_Count  : Float := 0.0;
   begin
      for index_k in Positive range 1 .. Criteria.Num_Outputs loop
         Sq_Count := 0.0;
         Sum_Total := Criteria.Sum_Total.Element (index_k);
         if Natural (Criteria.Classes.Length) > Criteria.Stride then
            Criteria.Stride  := Natural (Criteria.Classes.Length);
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

   procedure Init (Criteria                          : in out Criterion_Class;
                   Y                                 : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight                     : Classifier_Types.Natural_List;
                   Num_Weighted_Samples, Start, Stop : Natural;
                   Sample_Indices                    : Classifier_Types.Natural_List) is
      Y_I             : ML_Types.Value_Data_List;
      Sum_Total       : Classifier_Types.Natural_List;
      Sum_Total_Index : Natural := 1;
      i               : Natural;
      w               : Positive := 1;
      c               : ML_Types.Value_Record;
   begin
      Criteria.Y := Y;
      Criteria.Sample_Weight := Sample_Weight;
      Criteria.Num_Weighted_Samples := Num_Weighted_Samples;
      Criteria.Sample_Indices := Sample_Indices;
      Criteria.Start := Start;
      Criteria.Stop := Stop;
      Criteria.Sample_Indices := Sample_Indices;

      Criteria.Num_Node_Samples := Stop - Start;
      Criteria.Num_Weighted_Node_Samples := 0;
      Criteria.Pos := Criteria.Start;
      Criteria.Num_Weighted_Left := 0;
      Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

      for k in 1 .. Criteria.Num_Outputs loop
         Sum_Total := Criteria.Sum_Total.Element (k);
         for index in 1 .. Criteria.Classes.Length loop
            Sum_Total.Replace_Element (Sum_Total_Index, 0);
            Sum_Total_Index := Sum_Total_Index + Criteria.Stride;
         end loop;
         Criteria.Sum_Total.Replace_Element (k, Sum_Total);
      end loop;

      Criteria.Weighted_Node_Samples := 0;
      for p in Start .. Stop loop
         Sum_Total := Criteria.Sum_Total.Element (p);
         i := Criteria.Sample_Indices.Element (p);
         Y_I := Y.Element (i);
         if not Sample_Weight.Is_Empty then
            w := Sample_Weight.Element (i);
         end if;

         for k in 1 .. Criteria.Num_Outputs loop
            Sum_Total := Criteria.Sum_Total.Element (p);
            c := Y_I.Element (k);
            Sum_Total_Index := c.Integer_Value;
            Sum_Total.Replace_Element
              (Sum_Total_Index,
               Sum_Total.Element (Sum_Total_Index + w));
            Criteria.Sum_Total.Replace_Element (k, Sum_Total);
         end loop;

         Criteria.Weighted_Node_Samples :=
           Criteria.Weighted_Node_Samples + w;
      end loop;

      Reset (Criteria);

   end Init;

   --  ------------------------------------------------------------------------

   procedure Reset (Criteria : in out Criterion_Class) is
      Sum_Left         : Classifier_Types.Natural_List;
      Sum_Right        : Classifier_Types.Natural_List;
      Sum_Total        : Classifier_Types.Natural_List;
   begin
      Criteria.Pos := Criteria.Start;
      Criteria.Num_Weighted_Left := 0;
      Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

      for k in 1 .. Criteria.Num_Outputs loop
         Sum_Left := Criteria.Sum_Left.Element (k);
         Sum_Right := Criteria.Sum_Right.Element (k);
         Sum_Total := Criteria.Sum_Total.Element (k);

         for index in Positive range 1 .. Positive (Criteria.Classes.Length) loop
            Sum_Left.Replace_Element (index, 0);
            Sum_Right.Replace_Element
              (index, Sum_Total.Element (index));
         end loop;

         Criteria.Sum_Left.Replace_Element (k, Sum_Left);
         Criteria.Sum_Right.Replace_Element (k, Sum_Right);

      end loop;

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
      Sum_Left    : Classifier_Types.Natural_List :=
                        Criteria.Sum_Left.Element (1);
      Sum_Right   : Classifier_Types.Natural_List;
      Sum_Total   : Classifier_Types.Natural_List;
      i           : Positive;
      Label_Index : Positive;
      Values      : Value_Data_List;
      Weight      : Natural := 0;
   begin
      if (New_Pos - Criteria.Pos) <= (Criteria.Stop - New_Pos) then
         for p in Criteria.Pos .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Values := Criteria.Y.Element (i);
            for k in 1 .. Criteria.Num_Outputs loop
               Label_Index := (k -1) * Criteria.Stride +
                 Values.Element (k).Integer_Value;
               Sum_Left.Replace_Element
                 (Label_Index, Sum_Left.Element (Label_Index) + Weight);
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
               Label_Index := (k -1) * Criteria.Stride +
                 Values.Element (k).Integer_Value;
               Sum_Left.Replace_Element
                 (Label_Index, Sum_Left.Element (Label_Index) - Weight);
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
