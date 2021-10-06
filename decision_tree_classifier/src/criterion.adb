
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Classifier_Utilities;
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
      --  L771
      for k in 1 .. Num_Outputs loop
         Criteria.Sum_Total.Append (0.0);
      end loop;

      --  L773
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

            Criteria.Sum_Total.Replace_Element
              (k, Criteria.Sum_Total.Element (k) + W_Ik);
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
      Num_Outputs    : constant Positive := Positive (Criteria.Y.Length);
      Count_K        : Float;
      Gini           : Float := 0.0;
      Sq_Count       : Float := 0.0;
   begin
      --  623
      for index_k in Criteria.Y.First_Index .. Criteria.Y.Last_Index loop
         Sq_Count := 0.0;
         for Class_Index in Criteria.Classes.First_Index ..
           Criteria.Classes.Last_Index loop
            Count_K := Float (Criteria.Sum_Total.Element (Class_Index));
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
      Class_List     : ML_Types.Value_Data_List;
      Count_K        : Float := 0.0;
      Entropy        : Float := 0.0;
   begin
      if Self.Classes.Is_Empty then
         raise Criterion_Error with
           "Criterion.Entropy_Node_Impurity Criterion Classes is empty";
      end if;
      Put_Line ("Criterion.Entropy_Node_Impurity");
      Put_Line ("Criterion.Entropy_Node_Impurity Y: " &
                  Integer'Image (Integer (Self.Y.Length)) & " x" &
                  Integer'Image (Integer (Self.Y.Element (1).Length)));
      Put_Line ("Criterion.Entropy_Node_Impurity Classes length: " &
                  Integer'Image (Integer (Self.Classes.Length)));
      Put_Line ("Criterion.Entropy_Node_Impurity Classes (1) length: " &
                  Integer'Image (Integer (Self.Classes.Element (1).Length)));
      --  Y structure samples (rows) x outputs (columns)
      for index in Self.Y.Element (1).First_Index ..
        Self.Y.Element (1).Last_Index loop
         Put_Line ("Criterion.Entropy_Node_Impurity index" & Integer'Image (index));
         Class_List := Self.Classes.Element (index);
         for c in Class_List.First_Index .. Class_List.Last_Index loop
            Put_Line ("Criterion.Entropy_Node_Impurity c" & Integer'Image (index));
            Count_K := Self.Sum_Total.Element (c);
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
      Criteria.Classes.Clear;
      for index in Classes.First_Index .. Classes.Last_Index loop
         Criteria.Classes.Append (Classes.Element (index));
      end loop;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Classifier_Types.Float_List) is
   begin
      Value.Clear;
      for index in Self.Sum_Total.First_Index .. Self.Sum_Total.Last_Index loop
         Value.Append (Self.Sum_Total.Element (index) /
                         Self.Weighted_Node_Samples);
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
      for k in 1 .. Num_Outputs loop
         Sum := Criteria.Sum_Total.Element (k);
         for s in Values.First_Index .. Values.Last_Index loop
            Sum := Sum - Float (Values.Element (s)) * Weight;
         end loop;
         Criteria.Sum_Right.Replace_Element (k, Sum);
      end loop;

   end Update;

   --  ------------------------------------------------------------------------

end Criterion;
