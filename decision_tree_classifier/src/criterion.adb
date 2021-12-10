--  Based on scikit-learn/sklearn/tree _criterion.pyx
--  class ClassificationCriterion(Criterion)

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with ML_Types;
--  with Printing;

package body Criterion is

   --  -------------------------------------------------------------------------
   --  L214 __cinit__
   procedure C_Init (Criteria    : in out Criterion_Class;
                     Num_Outputs : Tree.Index_Range := 1;
                     Num_Classes : Classifier_Types.Natural_List :=
                       Classifier_Types.Natural_Package.Empty_Vector) is
   begin
      --  L220
      Criteria.Num_Outputs := Num_Outputs;
      Criteria.Num_Classes := Num_Classes;

   end C_Init;

   --  ------------------------------------------------------------------------
   --  L630
   procedure Children_Impurity_Gini (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float) is
      Routine_Name   : constant String := "Criterion.Gini_Children_Impurity ";
      Num_Outputs    : constant Positive := Positive (Criteria.Num_Outputs);
      Num_Classes    :  constant Classifier_Types.Natural_List :=
                         Criteria.Num_Classes;
      Sum_Left_K     : Classifier_Types.Float_List;
      Sum_Right_K    : Classifier_Types.Float_List;
      Count_K        : Float;
      Sq_Count_Left  : Float;
      Sq_Count_Right : Float;
      Gini_Left      : Float := 0.0;
      Gini_Right     : Float := 0.0;
   begin
      Assert (Criteria.Num_Weighted_Left > 0.0,
              Routine_Name & "Criteria.Num_Weighted_Left " &
                Float'Image (Criteria.Num_Weighted_Left) &
                " should be > 0.0.");
      Assert (Criteria.Num_Weighted_Right > 0.0,
              Routine_Name & "Criteria.Num_Weighted_Right " &
                Float'Image (Criteria.Num_Weighted_Right) &
                " should be > 0.0.");
      --  L662
      for k in 1 .. Num_Outputs loop
         Sq_Count_Left := 0.0;
         Sq_Count_Right := 0.0;
         Sum_Left_K := Criteria.Sum_Left.Element (k);
         Sum_Right_K := Criteria.Sum_Right.Element (k);

         for c in 1 .. Num_Classes.Element (k) loop
            Count_K := Sum_Left_K.Element (c);
            Sq_Count_Left := Sq_Count_Left + Count_K ** 2;

            Count_K := Sum_Right_K.Element (c);
            Sq_Count_Right := Sq_Count_Right + Count_K ** 2;
         end loop;
      end loop;

      Gini_Left := Gini_Left + 1.0 -
        Sq_Count_Left / Criteria.Num_Weighted_Left ** 2;
      Gini_Right := Gini_Right + 1.0 -
        Sq_Count_Left / Criteria.Num_Weighted_Right ** 2;

      Impurity_Left := Gini_Left / Float (Num_Outputs);
      Impurity_Right := Gini_Right / Float (Num_Outputs);

   end Children_Impurity_Gini;

   --  ------------------------------------------------------------------------
   --  L59, L214, 276
   procedure Classification_Init
     (Criteria            : in out Criterion_Class;
      Y                   : Classifier_Types.Natural_Lists_2D;
      Sample_Indices      : Classifier_Types.Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight       : Weights.Weight_List;
      Weighted_Samples    : Float;
      Start_Row, Stop_Row : Natural) is
      --  In Python a[start:stop] means items start through stop - 1
      --        Routine_Name    : constant String := "Criterion.Classification_Init ";
      Num_Outputs     : constant Positive := Positive (Y.Element (1).Length);
      Sum_Total_K     : Classifier_Types.Float_List;
      Y_I_Index       : Positive;  --  Class index
      Y_I             : Classifier_Types.Natural_List;  --  Class
      Y_Ik            : Natural; --  Class.output
      Weight          : Float := 1.0;
   begin
      --  L302
      Criteria.Y := Y;
      Criteria.Sample_Weight := Sample_Weight;
      Criteria.Sample_Indices := Sample_Indices;
      Criteria.Start_Row := Start_Row;
      Criteria.Stop_Row := Stop_Row;
      Criteria.Num_Node_Samples := Stop_Row - Start_Row + 1;
      Criteria.Num_Weighted_Samples := Weighted_Samples;
      Criteria.Num_Weighted_Node_Samples := 0.0;
      Criteria.Sum_Total.Clear;

      Assert (not Criteria.Num_Classes.Is_Empty,
              "Criterion.Classification_Init Criteria.Num_Classes is empty");
      --  L325 Initialize Sum_Total
      --  Sum_Total dimensions: num outputs x num classes
      for row in 1 .. Num_Outputs loop
         Sum_Total_K.Clear;
         for c in 1 .. Criteria.Num_Classes.Element (row) loop
            Sum_Total_K.Append (0.0);
         end loop;
         Criteria.Sum_Total.Append (Sum_Total_K);
      end loop;

      --  L325
      for p in Start_Row .. Stop_Row loop
         Y_I_Index := Sample_Indices.Element (p);

         --  Weight is originally set to be 1.0, meaning that if no
         --  sample weights are given the default weight of each sample is 1.0
         if not Sample_Weight.Is_Empty then
            Weight := Sample_Weight.Element (Y_I_Index);
         end if;

         --  L338 Count weighted class frequency for each target
         --  Y_I is Class
         Y_I := Y.Element (Y_I_Index);
         for k in 1 .. Num_Outputs loop
            Sum_Total_K := Criteria.Sum_Total.Element (k);
            --  L339 c = Y_Ik is an index into Y (output k) class i
            Y_Ik := Y_I.Element (k);   --  output.class
            --  sum_total[k * self.sum_stride + c] += w
            --  Add Weight to Y (output k, class Y_I)
            Sum_Total_K.Replace_Element
              (Y_Ik, Sum_Total_K.Element (Y_Ik) + Weight);
            Criteria.Sum_Total.Replace_Element (k, Sum_Total_K);
         end loop;

         Criteria.Num_Weighted_Node_Samples :=
           Criteria.Num_Weighted_Node_Samples + Weight;
      end loop;

      Reset (Criteria);

   end Classification_Init;

   --  ------------------------------------------------------------------------

   function Impurity_Improvement (Criteria       : Criterion_Class;
                                  Impurity_Parent, Impurity_Left,
                                  Impurity_Right : Float) return float is
      Routine_Name          : constant String := "Criterion.Impurity_Improvement";
      Weighted_Node_Samples : constant Float :=
                                Criteria.Num_Weighted_Node_Samples;
      Right_Component       : constant Float
        := (Criteria.Num_Weighted_Right / Weighted_Node_Samples) * Impurity_Right;
      Left_Component        : constant Float
        := (Criteria.Num_Weighted_Left / Weighted_Node_Samples) * Impurity_Left;
   begin
      Assert (Weighted_Node_Samples > 0.0,
              Routine_Name & "Criteria.Weighted_Node_Samples " &
                Float'Image (Weighted_Node_Samples) & " should be > 0.0");
      Assert (Criteria.Num_Weighted_Samples > 0.0,
              Routine_Name & "Criteria.Num_Weighted_Samples " &
                Float'Image (Criteria.Num_Weighted_Samples) &
                " should be > 0.0");
      return (Weighted_Node_Samples / Criteria.Num_Weighted_Samples) *
        (Impurity_Parent - Right_Component - Left_Component);

   end Impurity_Improvement;

   --  ------------------------------------------------------------------------
   --  L 605 Node_Impurity_Gini evaluates the Gini criterion as the impurity
   --  of the current node.
   --  Gini impurity is calculated by subtracting the sum of the squared
   --  probabilities of each class from one
   function Node_Impurity_Gini (Criteria : Criterion_Class) return Float is
      --        Routine_Name   : constant String := "Criterion.Node_Impurity_Gini ";
      Num_Outputs    : constant Positive := Positive (Criteria.Num_Outputs);
      Num_Classes    : constant Classifier_Types.Natural_List :=
                         Criteria.Num_Classes;
      Sum_Total_K    : Weights.Weight_List;
      Count_K        : Float;
      Gini           : Float := 0.0;
      Sq_Count       : Float := 0.0;
   begin
      --  L620
      for index_k in 1 .. Criteria.Num_Outputs loop
         Sq_Count := 0.0;
         Sum_Total_K := Criteria.Sum_Total.Element (Integer (index_k));

         for class_index in 1 .. Num_Classes.Element (Integer (index_k)) loop
            Count_K := Float (Sum_Total_K.Element (class_index));
            Sq_Count := Sq_Count + Count_K ** 2;
         end loop;

         Gini := Gini + 1.0 -
           Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
      end loop;

      return Gini / Float (Num_Outputs);

   end Node_Impurity_Gini;

   --  ------------------------------------------------------------------------
   --  L521 Node_Impurity_Entropy evaluates the cross-entropy criterion as
   --  impurity of the current node.  i.e. the impurity of samples[start:end].
   --  The smaller the impurity the better.
   function Node_Impurity_Entropy (Self : Criterion_Class) return Float is
      use Maths.Float_Math_Functions;
      Num_Classes : constant Classifier_Types.Natural_List := Self.Num_Classes;
      Sum_Total_K : Classifier_Types.Float_List;
      Count_K     : Float := 0.0;
      Entropy     : Float := 0.0;
   begin
      Assert (not Self.Num_Classes.Is_Empty,
              "Criterion.Entropy_Node_Impurity Criterion Num_Classes is empty");

      --  L535 Y structure samples (rows) x outputs (columns)
      --  k in range(self.n_outputs)
      for index_k in Self.Y.Element (1).First_Index
        .. Self.Y.Element (1).Last_Index loop
         Sum_Total_K := Self.Sum_Total.Element (index_k);
         for c in 1 .. Num_Classes.Element (index_k) loop
            Count_K := Sum_Total_K.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Self.Num_Weighted_Node_Samples;
               Entropy := Entropy - Count_K * Log (Count_K);
            end if;
         end loop;
      end loop;

      return Entropy / Float (Self.Sum_Total.Length);

   end Node_Impurity_Entropy;

   --  ------------------------------------------------------------------------
   --  L200
   --  L490  ClassificationCriterion(Criterion).node_value
   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Weights.Weight_Lists_2D) is
   begin
      Assert (not Self.Sum_Total.Is_Empty,
              "Criterion.Node_Value Self.Sum_Total is empty");
      --  Value dimensions: num outputs x num classes
      Value := Self.Sum_Total;

   end Node_Value;

   --  -------------------------------------------------------------------------

   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                        return Float is
      Impurity_Left  : Float;
      Impurity_Right : Float;
   begin
      Children_Impurity_Gini (Criteria, Impurity_Left, Impurity_Right);
      return -Criteria.Num_Weighted_Right * Impurity_Right -
        Criteria.Num_Weighted_Left * Impurity_Left;

   end Proxy_Impurity_Improvement;

   --  ------------------------------------------------------------------------
   --  L348 Reset the criterion to pos=start
   procedure Reset (Criteria : in out Criterion_Class) is
      Num_Outputs : constant Positive :=
                      Positive (Criteria.Y.Element (1).Length);
      Sum_Left_K  : Classifier_Types.Float_List;
   begin
      Criteria.Split_Row := Criteria.Start_Row;
      Criteria.Num_Weighted_Left := 0.0;
      Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

      Criteria.Sum_Left.Clear;
      Criteria.Sum_Right.Clear;
      for k in 1 .. Num_Outputs loop
         Sum_Left_K.Clear;
         for c in 1 .. Criteria.Num_Classes.Element (k) loop
            Sum_Left_K.Append (0.0);
         end loop;
         Criteria.Sum_Left.Append (Sum_Left_K);
      end loop;
      Criteria.Sum_Right := Criteria.Sum_Total;

   end Reset;

   --  ------------------------------------------------------------------------
   --  L378
   procedure Reverse_Reset (Criteria : in out Criterion_Class) is
      Num_Outputs : constant Positive :=
                      Positive (Criteria.Y.Element (1).Length);
      Sum_Right_K : Classifier_Types.Float_List;
   begin
      Criteria.Split_Row := Criteria.Stop_Row + 1;
      Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Node_Samples;
      Criteria.Num_Weighted_Right := 0.0;

      Criteria.Sum_Right.Clear;
      for index in 1 .. Num_Outputs loop
         Sum_Right_K.Clear;
         for c in 1 .. Criteria.Num_Classes.Element (index) loop
            Sum_Right_K.Append (0.0);
         end loop;
         Criteria.Sum_Right.Append (Sum_Right_K);
      end loop;
      Criteria.Sum_Left := Criteria.Sum_Total;

   end Reverse_Reset;

   --  ------------------------------------------------------------------------
   --  L398 Update statistics by moving samples[pos:new_pos] to the left child.
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive) is
--        Routine_Name : constant String := "Criterion.Update ";
      Num_Outputs  : constant Positive :=
                       Positive (Criteria.Y.Element (1).Length);
      i            : Positive;
      Y_I          : Classifier_Types.Natural_List;
      Label_Index  : Positive;  --  Class index?
      Sum_Left_K   : Classifier_Types.Float_List;
      Sum_Right_K  : Classifier_Types.Float_List;
      Sum_K        : Classifier_Types.Float_List;
      Weight       : Float := 1.0;
   begin
      --  L435  Update statistics up to new_pos given that
      --  sum_left[x] +  sum_right[x] = sum_total[x] and that sum_total
      --  is known, update sum_left from the direction that requires
      --  the least amount of computations.
      --  Criteria.Split_Row is the start position of the current right side.
      --  New_Pos is the start position of the updated right side.
      --  New_Pos - 1 is the new ending position for which to move samples from
      --  the right child to the left child.
      --  Move Criteria.Split_Row .. New_Pos - 1 to left side

      if (New_Pos - Criteria.Split_Row) <= (Criteria.Stop_Row - New_Pos) then
         for p in Criteria.Split_Row .. New_Pos - 1 loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Y_I := Criteria.Y.Element (i);
            for k in 1 .. Num_Outputs loop
               Sum_Left_K := Criteria.Sum_Left.Element (k);
               Label_Index := Y_I.Element (k);  --  Class index
               Sum_Left_K.Replace_Element
                 (Label_Index, Sum_Left_K.Element (Label_Index) + Weight);
               Criteria.Sum_Left.Replace_Element (k, Sum_Left_K);
            end loop;
            Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left + Weight;
         end loop;

      else  --  L449
         Reverse_Reset (Criteria);
         --  Num_Weighted_Left set to Num_Weighted_Node_Samples by Reverse_Reset
         for p in reverse New_Pos .. Criteria.Stop_Row loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            --  L446
            Y_I := Criteria.Y.Element (i);
            for k in 1 .. Num_Outputs loop
               Label_Index := Y_I.Element (k);
               Sum_Left_K := Criteria.Sum_Left.Element (k);
               Sum_Left_K.Replace_Element
                 (Label_Index, Sum_Left_K.Element (Label_Index) - Weight);
               Criteria.Sum_Left.Replace_Element (k, Sum_Left_K);
            end loop;

            Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left - Weight;
         end loop;
      end if;

      --  L467 Update right part statistics
      Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples -
        Criteria.Num_Weighted_Left;
      for k in 1 .. Num_Outputs loop
         Sum_Left_K := Criteria.Sum_Left.Element (k);
         Sum_Right_K := Criteria.Sum_Right.Element (k);
         Sum_K := Criteria.Sum_Total.Element (k);
         for class_index in 1 .. Criteria.Num_Classes.Element (k) loop
            Sum_Right_K.Replace_Element (class_index, Sum_K.Element (class_index) -
                                           Sum_Left_K.Element (class_index));
         end loop;
         Criteria.Sum_Right.Replace_Element (k, Sum_Right_K);
      end loop;
      Criteria.Split_Row := New_Pos;

   end Update;

   --  ------------------------------------------------------------------------

end Criterion;
