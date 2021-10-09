
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Utilities;
with ML_Types;

package body Criterion is

   --  ------------------------------------------------------------------------
   --  L59, L214, 280
   procedure Classification_Init
     (Criteria         : in out Criterion_Class;
      Y                : Classifier_Types.List_Of_Natural_Lists;
      Sample_Indices   : Classifier_Types.Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight    : Weights.Weight_List;
      Weighted_Samples : Float;
      Start, Stop      : Natural) is
      Num_Outputs  : constant Positive := Positive (Y.Element (1).Length);
      Sum_Total_K  : Weights.Weight_List;
      Y_I_Index    : Positive;
      Y_I          : Classifier_Types.Natural_List;
      Y_Ik         : Natural;
      Weight       : Float := 1.0;
      W_Ik         : Float;
   begin
      Criteria.Y := Y;
      Criteria.Sample_Weight := Sample_Weight;
      Criteria.Sample_Indices := Sample_Indices;
      Criteria.Start := Start;
      Criteria.Stop := Stop;
      Criteria.Weighted_Samples := Weighted_Samples;
      Criteria.Num_Weighted_Node_Samples := 0;

      Criteria.Sq_Sum_Total := 0.0;
      Criteria.Sum_Total.Clear;
      --  L325
      for row in 1 .. Num_Outputs loop
         Sum_Total_K.Clear;
         for col in 1 .. Criteria.Classes.Element (row).Length loop
            Sum_Total_K.Append (0.0);
         end loop;
         Criteria.Sum_Total.Append (Sum_Total_K);
      end loop;

      --  L329
      for p in Start .. Stop loop
         Y_I_Index := Sample_Indices.Element (p);

         --  Weight is originally set to be 1.0, meaning that if no
         --  sample weights are given, the default weight of each sample is 1.0
         if not Sample_Weight.Is_Empty then
            Weight := Sample_Weight.Element (Y_I_Index);
         end if;

         Y_I := Y.Element (Y_I_Index);
         for k in 1 .. Num_Outputs loop
            Y_Ik := Y_I.Element (k);
            W_Ik := Float (Y_Ik) * Weight;
            Sum_Total_K := Criteria.Sum_Total.Element (k);
            for col in 1 .. Positive (Criteria.Classes.Element (k).Length) loop
               Sum_Total_K.Replace_Element (col, Sum_Total_K.Element (col) + W_Ik);
            end loop;
            Criteria.Sum_Total.Replace_Element (k, Sum_Total_K);
            Criteria.Sq_Sum_Total :=
              Criteria.Sq_Sum_Total + Float (Y_Ik) * W_Ik;
         end loop;

         Criteria.Weighted_Node_Samples :=
           Criteria.Weighted_Node_Samples + Weight;
      end loop;

      Reset (Criteria);

   end Classification_Init;

   --  ------------------------------------------------------------------------
   --  L637
   procedure Gini_Children_Impurity (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float) is
      use Maths.Float_Math_Functions;
      Num_Outputs    : constant Positive := Positive (Criteria.Y.Length);
      Class_List     : Classifier_Types.Natural_List;
      Count_K        : Float;
      Entropy_Left   : Float := 0.0;
      Entropy_Right  : Float := 0.0;
   begin
      --  L662
      for k in Criteria.Y.First_Index .. Criteria.Y.Last_Index loop
         Class_List := Criteria.Y.Element (k);
         for c in Class_List.First_Index .. Class_List.Last_Index loop
            Count_K := Criteria.Sum_Left.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Criteria.Weighted_Left;
               Entropy_Left := Entropy_Left - Count_K * Log (Count_K);
            end if;

            Count_K := Criteria.Sum_Right.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Criteria.Weighted_Right;
               Entropy_Right := Entropy_Right - Count_K * Log (Count_K);
            end if;
         end loop;
      end loop;

      Impurity_Left := Entropy_Left / Float (Num_Outputs);
      Impurity_Right := Entropy_Right / Float (Num_Outputs);

   end Gini_Children_Impurity;

   --  ------------------------------------------------------------------------
   --  L 608 Gini_Node_Impurity evaluates the Gini criterion as the impurity
   --   of the current node
   function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                return Float is
      Num_Outputs   : constant Positive := Positive (Criteria.Y.Length);
      Sum_Total_K   : Weights.Weight_List;
      Count_K       : Float;
      Gini          : Float := 0.0;
      Sq_Count      : Float := 0.0;
   begin
      --  623
      for index_k in Criteria.Y.First_Index .. Criteria.Y.Last_Index loop
         Sq_Count := 0.0;
         for op_index in 1 .. Num_Outputs loop
            Sum_Total_K := Criteria.Sum_Total.Element (op_index);
            for Class_Index in Criteria.Classes.First_Index ..
              Criteria.Classes.Last_Index loop
               Count_K := Float (Sum_Total_K.Element (Class_Index));
            end loop;
            Sq_Count := Sq_Count + Count_K ** 2;
         end loop;

         Gini := Gini +
           1.0 - Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
      end loop;

      return Gini / Float (Num_Outputs);

   end Gini_Node_Impurity;

   --  ------------------------------------------------------------------------
   --  L524 Entropy_Node_Impurity evaluate the cross-entropy criterion as impurity of the
   --  current node. i.e. the impurity of samples[start:end].
   --  The smaller the impurity the better.
   function Entropy_Node_Impurity (Self : Criterion_Class) return Float is
      use Maths.Float_Math_Functions;
      Class_List  : ML_Types.Value_Data_List;
      Sum_Total_K : Weights.Weight_List;
      Count_K     : Float := 0.0;
      Entropy     : Float := 0.0;
   begin
      if Self.Classes.Is_Empty then
         raise Criterion_Error with
           "Criterion.Entropy_Node_Impurity Criterion Classes is empty";
      end if;

      Classifier_Utilities.Print_List_Of_Value_Lists ("Classes", Self.Classes);
      --  Y structure samples (rows) x outputs (columns)
      for index in Self.Y.Element (1).First_Index .. Self.Y.Element (1).Last_Index loop
         Class_List := Self.Classes.Element (index);
         Sum_Total_K := Self.Sum_Total.Element (index);
         for c in Class_List.First_Index .. Class_List.Last_Index loop
            Count_K := Sum_Total_K.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Self.Weighted_Node_Samples;
               Entropy := Entropy - Count_K * Log (Count_K);
            end if;
         end loop;
      end loop;

      return Entropy / Float (Self.Sum_Total.Length);

   end Entropy_Node_Impurity;

   --  ------------------------------------------------------------------------

   function Impurity_Improvement
     (Criteria                                       : Criterion_Class;
      Impurity_Parent, Impurity_Left, Impurity_Right : Float) return float is
   begin

      return (Criteria.Weighted_Node_Samples / Criteria.Weighted_Samples) *
        (Impurity_Parent -
           (Criteria.Weighted_Right / Criteria.Weighted_Node_Samples *
                Impurity_Right) -
           (Criteria.Weighted_Left / Criteria.Weighted_Node_Samples *
                Impurity_Left));

   end Impurity_Improvement;

   --  ------------------------------------------------------------------------
   --  L214 __cinit__
   procedure Init (Criteria : in out Criterion_Class;
                   Classes  : ML_Types.List_Of_Value_Data_Lists) is
   begin
      --  L252
      Criteria.Classes := Classes;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Classifier_Types.List_Of_Float_Lists) is
      Sum_Total_K : Weights.Weight_List;
      Value_K     : Classifier_Types.Float_List;
   begin
      Value.Clear;
      for index in Self.Sum_Total.First_Index .. Self.Sum_Total.Last_Index loop
         Sum_Total_K := Self.Sum_Total.Element (index);
         Value_K.Clear;
         for index_2 in Self.Sum_Total.First_Index ..
           Self.Sum_Total.Last_Index loop
            Value_K.Append (Sum_Total_K.Element (index_2) /
                              Self.Weighted_Node_Samples);
         end loop;
         Value.Append (Value_K);
      end loop;

   end Node_Value;

   --  -------------------------------------------------------------------------

   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                        return Float is
      Impurity_Left  : Float;
      Impurity_Right : Float;
   begin
      Gini_Children_Impurity (Criteria, Impurity_Left, Impurity_Right);
      return -Criteria.Weighted_Right * Impurity_Right -
        Criteria.Weighted_Left * Impurity_Left;

   end Proxy_Impurity_Improvement;

   --  ------------------------------------------------------------------------
   --  L348
   procedure Reset (Criteria : in out Criterion_Class) is
      Num_Outputs  : constant Positive :=
                       Positive (Criteria.Y.Element (1).Length);
   begin
      Criteria.Sum_Left.Clear;
      Criteria.Sum_Right.Clear;
      for k in 1 .. Num_Outputs loop
         Criteria.Sum_Left.Append (0.0);
         Criteria.Sum_Right.Append (Criteria.Sum_Total.Element (k));
      end loop;

      Criteria.Weighted_Left := 0.0;
      Criteria.Weighted_Right := Criteria.Weighted_Node_Samples;

   end Reset;

   --  ------------------------------------------------------------------------

   procedure Reverse_Reset (Criteria : in out Criterion_Class) is
      Num_Outputs  : constant Positive :=
                       Positive (Criteria.Y.Element (1).Length);
   begin
      Criteria.Sum_Left.Clear;
      Criteria.Sum_Right.Clear;
      for index in 1 .. Num_Outputs loop
         Criteria.Sum_Right.Append (0.0);
         Criteria.Sum_Left.Append (Criteria.Sum_Total (index));
      end loop;

   end Reverse_Reset;

   --  ------------------------------------------------------------------------
   --  Update statistics by moving samples[pos:new_pos] to the left child.
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive) is
      Num_Outputs  : constant Positive := Positive (Criteria.Y.Length);
      Sum_Total_K  : Weights.Weight_List;
      Sum          : Float;
      i            : Positive;
      Values       : Classifier_Types.Natural_List;
      Weight       : Float := 1.0;
   begin
      --  L439
      if (New_Pos - Criteria.Position) <= (Criteria.Stop - New_Pos) then
         for p in Criteria.Position .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Values := Criteria.Y.Element (i);
            for k in Criteria.Sum_Left.First_Index ..
              Criteria.Sum_Left.Last_Index loop
               Sum := Criteria.Sum_Left.Element (k);
               for s in Values.First_Index .. Values.Last_Index loop
                  Sum := Sum + Float (Values.Element (s)) * Weight;
               end loop;
               Criteria.Sum_Left.Replace_Element (k, Sum);
            end loop;

            Criteria.Weighted_Left := Criteria.Weighted_Left + Weight;
         end loop;

      else
         Reverse_Reset (Criteria);
         for p in reverse Criteria.Stop .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Values := Criteria.Y.Element (i);
            for k in Criteria.Sum_Left.First_Index ..
              Criteria.Sum_Left.Last_Index loop
               Sum := Criteria.Sum_Left.Element (k);
               for s in Values.First_Index .. Values.Last_Index loop
                  Sum := Sum - Float (Values.Element (s)) * Weight;
               end loop;
               Criteria.Sum_Left.Replace_Element (k, Sum);
            end loop;

            Criteria.Weighted_Left := Criteria.Weighted_Left - Weight;
         end loop;
      end if;

      --  Update right part statistics
      Criteria.Weighted_Right := Criteria.Weighted_Node_Samples -
        Criteria.Weighted_Left;
      Sum := 0.0;
      for k in 1 .. Num_Outputs loop
         Sum_Total_K := Criteria.Sum_Total.Element (k);
         for k2 in Criteria.Sum_Total.Element (1).First_Index ..
           Criteria.Sum_Total.Element (1).Last_Index loop
            Sum := Sum + Sum_Total_K.Element (k2);
         end loop;

         for s in Values.First_Index .. Values.Last_Index loop
            Sum := Sum - Float (Values.Element (s)) * Weight;
         end loop;
         Criteria.Sum_Right.Replace_Element (k, Sum);
      end loop;

   end Update;

   --  ------------------------------------------------------------------------

end Criterion;
