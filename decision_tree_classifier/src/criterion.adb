--  Based on scikit-learn/sklearn/tree _criterion.pyx
--  class ClassificationCriterion(Criterion)

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with ML_Types;
with Printing;

package body Criterion is

   --  -------------------------------------------------------------------------
   --  L214 __cinit__
   procedure C_Init (Criteria    : in out Criterion_Class;
                     Num_Outputs : Tree.Index_Range;
                     Num_Classes : Classifier_Types.Natural_List) is
      --                        Classes     : ML_Types.Value_Data_Lists_2D) is
   begin
      --  L252
      Criteria.Num_Outputs := Num_Outputs;
      Criteria.Num_Classes := Num_Classes;
      --          Criteria.Classes := Classes;

   end C_Init;

   --  ------------------------------------------------------------------------
   --  L59, L214, 280
   procedure Classification_Init
     (Criteria           : in out Criterion_Class;
      Y                  : Classifier_Types.List_Of_Natural_Lists;
      Sample_Indices     : Classifier_Types.Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight      : Weights.Weight_List;
      Weighted_Samples   : Float;
      Start_Row, End_Row : Natural) is
      Num_Outputs     : constant Positive := Positive (Y.Element (1).Length);
      Sum_Total_K     : Classifier_Types.Float_List;
      Y_I_Index       : Positive;
      Y_I             : Classifier_Types.Natural_List;
      Y_Ik            : Natural;
      Weight          : Float := 1.0;
   begin
      Criteria.Y := Y;
      Criteria.Sample_Weight := Sample_Weight;
      Criteria.Sample_Indices := Sample_Indices;
      Criteria.Start_Row := Start_Row;
      Criteria.End_Row := End_Row;
      Criteria.Num_Weighted_Samples := Weighted_Samples;
      Criteria.Num_Weighted_Node_Samples := 0.0;

      --        Criteria.Sq_Sum_Total := 0.0;
      Criteria.Sum_Total.Clear;

      Assert (not Criteria.Num_Classes.Is_Empty,
              "Criterion.Classification_Init Criteria.Num_Classes is empty");
      --  L325
      for row in 1 .. Num_Outputs loop
         Sum_Total_K.Clear;
         --              for c in Criteria.Classes.Element (row).First_Index ..
         --                Criteria.Classes.Element (row).Last_Index loop
         for c in 1 .. Criteria.Num_Classes.Element (row) loop
            Sum_Total_K.Append (0.0);
         end loop;
         Criteria.Sum_Total.Append (Sum_Total_K);
      end loop;

      Put_Line ("Criterion.Classification_Init Start_Row, End_Row " &
                  Integer'Image (Start_Row) & ", " & Integer'Image (End_Row));
      --  L329
      for p in Start_Row .. End_Row loop
         Y_I_Index := Sample_Indices.Element (p);

         --  Weight is originally set to be 1.0, meaning that if no
         --  sample weights are given, the default weight of each sample is 1.0
         if not Sample_Weight.Is_Empty then
            Weight := Sample_Weight.Element (Y_I_Index);
         end if;

         --  L338 Count weighted class frequency for each target
         Y_I := Y.Element (Y_I_Index);
         for k in 1 .. Num_Outputs loop
            Sum_Total_K := Criteria.Sum_Total.Element (k);
            --  Y_Ik is an index into Y (output k) classes
            Y_Ik := Y_I.Element (k);
            --              Put_Line ("Criterion.Classification_Init p, Y_Ik " &
            --                          Integer'Image (p) & ", " & Integer'Image (Y_Ik));
            --  L339 c = Y_Ik
            --  sum_total[k * self.sum_stride + c] += w
            --  Add Weight to Y (output k, class Y_Ik)
            Sum_Total_K.Replace_Element
              (Y_Ik, Sum_Total_K.Element (Y_Ik) + Weight);
            Criteria.Sum_Total.Replace_Element (k, Sum_Total_K);
         end loop;

         Criteria.Num_Weighted_Node_Samples :=
           Criteria.Num_Weighted_Node_Samples + Weight;
      end loop;

      Printing.Print_Weights_Lists_2D
        ("Criterion.Classification_Init Sum_Total of each class",
         Criteria.Sum_Total);

      Reset (Criteria);

      Put_Line ("Criterion.Classification_Init done");

   end Classification_Init;

   --  ------------------------------------------------------------------------
   --  L637
   procedure Gini_Children_Impurity (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float) is
      use Maths.Float_Math_Functions;
      Num_Outputs    : constant Positive :=
                         Positive (Criteria.Y.Element (1).Length);
      Class_List     : ML_Types.Value_Data_List;
      Sum_Left_K     : Classifier_Types.Float_List;
      Sum_Right_K    : Classifier_Types.Float_List;
      Count_K        : Float;
      Sq_Count_Left  : Float;
      Sq_Count_Right : Float;
   begin
      --  L662
      for k in Criteria.Num_Classes.First_Index ..
        Criteria.Num_Classes.Last_Index loop
         Sq_Count_Left := 0.0;
         Sq_Count_Right := 0.0;
         --              Class_List := Criteria.Classes.Element (k);
         Sum_Left_K := Criteria.Sum_Left.Element (k);
         Sum_Right_K := Criteria.Sum_Right.Element (k);
         for c in Class_List.First_Index .. Class_List.Last_Index loop
            Count_K := Sum_Left_K.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Criteria.Num_Weighted_Left;
               Sq_Count_Left := Sq_Count_Left - Count_K * Log (Count_K);
            end if;

            Count_K := Sum_Right_K.Element (c);
            if Count_K > 0.0 then
               Count_K := Count_K / Criteria.Num_Weighted_Right;
               Sq_Count_Right := Sq_Count_Right - Count_K * Log (Count_K);
            end if;
         end loop;
      end loop;

      Impurity_Left := Sq_Count_Left / Float (Num_Outputs);
      Impurity_Right := Sq_Count_Right / Float (Num_Outputs);

   end Gini_Children_Impurity;

   --  ------------------------------------------------------------------------
   --  L 608 Gini_Node_Impurity evaluates the Gini criterion as the impurity
   --   of the current node
   function Gini_Node_Impurity (Criteria : Criterion_Class) return Float is
      Num_Outputs   : constant Positive := Positive (Criteria.Num_Outputs);
      Num_Classes   : constant Classifier_Types.Natural_List :=
                        Criteria.Num_Classes;
      Sum_Total_K   : Weights.Weight_List;
      Classes_K     : Positive;
      Count_K       : Float;
      Gini          : Float := 0.0;
      Sq_Count      : Float := 0.0;
   begin
      --  L620
      Put_Line ("Criterion.Gini_Node_Impurity L620");
      Printing.Print_Weights_Lists_2D
        ("Criterion.Gini_Node_Impurity Sum_Total", Criteria.Sum_Total);
      for index_k in 1 .. Criteria.Num_Outputs loop
         Put_Line ("Criterion.Gini_Node_Impurity index_k" &
                     Integer'Image (Integer (index_k)));
         Sq_Count := 0.0;
         Put_Line ("Criterion.Gini_Node_Impurity, Num_Classes length" &
                     Integer'Image (Integer (Num_Classes.Length)));
         Classes_K := Num_Classes.Element (Integer (index_k));
         Put_Line ("Criterion.Gini_Node_Impurity, Classes_K" &
                     Integer'Image (Classes_K));
         Sum_Total_K := Criteria.Sum_Total.Element (Integer (index_k));
         Printing.Print_Weights
           ("Criterion.Gini_Node_Impurity Sum_Total_K", Sum_Total_K);
         for class_index in 1 .. Classes_K loop
            Put_Line ("Criterion.Gini_Node_Impurity, Class_Index" &
                        Integer'Image (class_index));
            Count_K := Float (Sum_Total_K.Element (class_index));
            Put_Line ("Criterion.Gini_Node_Impurity Count_K" &
                        Float'Image (Count_K));
         end loop;
         Put_Line ("Criterion.Gini_Node_Impurity inner loop done");
         Sq_Count := Sq_Count + Count_K ** 2;
         Put_Line ("Criterion.Gini_Node_Impurity Sq_Count, Num_Weighted_Node_Samples" &
                     Float'Image (Sq_Count) & ", " &
                     Float'Image (Criteria.Num_Weighted_Node_Samples));

         Gini := Gini +
           1.0 - Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
         Put_Line ("Criterion.Gini_Node_Impurity Gini" &
                     Float'Image (Gini));
      end loop;
      Put_Line ("Criterion.Gini_Node_Impurity loop done");
      Put_Line ("Criterion.Gini_Node_Impurity Gini, Num_Outputs" &
                  Float'Image (Gini) & ", " & Integer'Image (Num_Outputs));

      return Gini / Float (Num_Outputs);

   end Gini_Node_Impurity;

   --  ------------------------------------------------------------------------
   --  L524 Entropy_Node_Impurity evaluates the cross-entropy criterion as
   --  impurity of the current node.  i.e. the impurity of samples[start:end].
   --  The smaller the impurity the better.
   function Entropy_Node_Impurity (Self : Criterion_Class) return Float is
      use Maths.Float_Math_Functions;
      Class_List  : ML_Types.Value_Data_List;
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
         Put_Line ("Criterion.Entropy_Node_Impurity, index_k" &
                     Integer'Image (index_k));
         --              Class_List := Self.Classes.Element (index_k);
         Sum_Total_K := Self.Sum_Total.Element (index_k);
         for c in Class_List.First_Index .. Class_List.Last_Index loop
            Count_K := Sum_Total_K.Element (c);
            if Count_K > 0.0 then
               Put_Line ("Criterion.Entropy_Node_Impurity, Num_Weighted_Node_Samples" &
                           Float'Image (Self.Num_Weighted_Node_Samples));
               Count_K := Count_K / Self.Num_Weighted_Node_Samples;
               Put_Line ("Criterion.Entropy_Node_Impurity, Count_K" &
                           Float'Image (Count_K));
               Entropy := Entropy - Count_K * Log (Count_K);
            end if;
         end loop;
      end loop;
      Put_Line ("Criterion.Entropy_Node_Impurity, Entropy" &
                  Float'Image (Entropy / Float (Self.Sum_Total.Length)));

      return Entropy / Float (Self.Sum_Total.Length);

   end Entropy_Node_Impurity;

   --  ------------------------------------------------------------------------

   function Impurity_Improvement (Criteria       : Criterion_Class;
                                  Impurity_Parent, Impurity_Left,
                                  Impurity_Right : Float) return float is
   begin

      return (Criteria.Num_Weighted_Node_Samples / Criteria.Num_Weighted_Samples) *
        (Impurity_Parent -
           (Criteria.Num_Weighted_Right / Criteria.Num_Weighted_Node_Samples *
                Impurity_Right) -
           (Criteria.Num_Weighted_Left / Criteria.Num_Weighted_Node_Samples *
                Impurity_Left));

   end Impurity_Improvement;

   --  ------------------------------------------------------------------------
   --  L490  ClassificationCriterion(Criterion).node_value
   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Weights.Weight_Lists_2D) is
   begin
      Assert (not Self.Sum_Total.Is_Empty,
              "Criterion.Node_Value Self.Sum_Total is empty");

      Value := Self.Sum_Total;

   end Node_Value;

   --  -------------------------------------------------------------------------

   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                         return Float is
      Impurity_Left  : Float;
      Impurity_Right : Float;
   begin
      Gini_Children_Impurity (Criteria, Impurity_Left, Impurity_Right);
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
         --              for c in Criteria.Classes.Element (k).First_Index ..
         --                Criteria.Classes.Element (k).Last_Index loop
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
      Criteria.Split_Row := Criteria.End_Row;
      Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Node_Samples;
      Criteria.Num_Weighted_Right := 0.0;

      Criteria.Sum_Right.Clear;
      for index in 1 .. Num_Outputs loop
         Sum_Right_K.Clear;
         --              for c in Criteria.Classes.Element (index).First_Index ..
         --                Criteria.Classes.Element (index).Last_Index loop
         for c in 1 .. Criteria.Num_Classes.Element (index) loop
            Sum_Right_K.Append (0.0);
         end loop;
         Criteria.Sum_Right.Append (Sum_Right_K);
      end loop;
      Criteria.Sum_Left := Criteria.Sum_Total;

   end Reverse_Reset;

   --  ------------------------------------------------------------------------
   --  L402 Update statistics by moving samples[pos:new_pos] to the left child.
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive) is
      Num_Outputs : constant Positive :=
                      Positive (Criteria.Y.Element (1).Length);
      i           : Positive;
      Y_I         : Classifier_Types.Natural_List;
      Y_Ik        : Natural;  --  Class index
      Sum_Left_K  : Classifier_Types.Float_List;
      Sum_Right_K : Classifier_Types.Float_List;
      Sum_K       : Classifier_Types.Float_List;
      Weight      : Float := 1.0;
   begin
      --  L439
      if (New_Pos - Criteria.Split_Row) <= (Criteria.End_Row - New_Pos) then
         for p in Criteria.Split_Row .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Y_I := Criteria.Y.Element (i);
            for k in 1 .. Num_Outputs loop
               Sum_Left_K := Criteria.Sum_Left.Element (k);
               Y_Ik := Y_I.Element (k);  --  Class index
               Sum_Left_K.Replace_Element
                 (Y_Ik, Sum_Left_K.Element (Y_Ik) + Weight);
               Criteria.Sum_Left.Replace_Element (k, Sum_Left_K);
            end loop;
            Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left + Weight;
         end loop;

      else  --  L452
         Reverse_Reset (Criteria);
         for p in reverse Criteria.End_Row .. New_Pos loop
            i := Criteria.Sample_Indices.Element (p);
            if not Criteria.Sample_Weight.Is_Empty then
               Weight := Criteria.Sample_Weight.Element (i);
            end if;

            Y_I := Criteria.Y.Element (i);
            for k in 1 .. Num_Outputs loop
               Y_Ik := Y_I.Element (k);
               Sum_Left_K := Criteria.Sum_Left.Element (k);
               Sum_Left_K.Replace_Element
                 (Y_Ik, Sum_Left_K.Element (Y_Ik) - Weight);
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
         --              for class_index in Criteria.Classes.Element (k).First_Index ..
         --                Criteria.Classes.Element (k).Last_Index loop
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
