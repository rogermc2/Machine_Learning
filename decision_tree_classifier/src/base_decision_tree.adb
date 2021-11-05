--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Ada_Tree_Builder;
with Classifier_Types;
with Criterion;
with Encode_Utils;
with Printing;

package body Base_Decision_Tree is

   procedure Base_Fit_Checks
     (aClassifier    : in out Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y              : ML_Types.Value_Data_Lists_2D;
      Sample_Weights : in out Classifier_Types.Float_List);
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y                      : ML_Types.Value_Data_Lists_2D;
      Y_Encoded              : out Classifier_Types.List_Of_Natural_Lists;
      Classes                : out ML_Types.Value_Data_Lists_2D;
      Expanded_Class_Weights : in out Classifier_Types.Float_List);
   procedure Prune_Tree (aClassifier : in out Classifier);

   --  -------------------------------------------------------------------------
   --  if is_classification: part of _classes.py BasesDecisionTree.Fit
   --  L150
   procedure Base_Fit
     (aClassifier    : in out Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y              : ML_Types.Value_Data_Lists_2D;
      Sample_Weights : out Classifier_Types.Float_List;
      Max_Depth      : Integer := -1) is
      use Estimator;
      Criteria              : Criterion.Criterion_Class;
      Splitter              : Node_Splitter.Splitter_Class;
      Y_Encoded             : Classifier_Types.List_Of_Natural_Lists;
      Classes               : ML_Types.Value_Data_Lists_2D;
      Num_Classes           : Classifier_Types.Natural_List;
      --  L205
      Expanded_Class_Weight : Weights.Weight_List;
      Sum_Sample_Weight     : Float := 0.0;
   begin
      Assert (not X.Is_Empty,
           "Base_Decision_Tree.Base_Fit, X is empty");
      Assert (not Y.Is_Empty,
           "Base_Decision_Tree.Base_Fit, Y is empty");

      --  L207  Generates Y_Encoded and Classes
      if aClassifier.Estimator_Kind = Classifier_Estimator then
         Classification_Part (aClassifier, Y, Y_Encoded,
                              Classes, Expanded_Class_Weight);
      end if;

      for index in Classes.First_Index .. Classes.Last_Index loop
          Num_Classes.Append (Positive (Classes.Element (index).Length));
      end loop;

      Criterion.C_Init (Criteria, Tree.Index_Range (Y.Element (1).Length),
                        Num_Classes);
      --  L163
      Node_Splitter.C_Init (Splitter, Criteria);
      C_Init (aClassifier, Criteria, Splitter, Max_Depth => Max_Depth);

      --  L184
      aClassifier.Attributes.Num_Features :=
        Tree.Index_Range (X.Element (1).Length);
      Node_Splitter.Init (Splitter, X, Y_Encoded, Sample_Weights);

      Base_Fit_Checks (aClassifier, X, Y, Sample_Weights);
      if Expanded_Class_Weight.Is_Empty then
         if Sample_Weights.Is_Empty then
            Sample_Weights := Expanded_Class_Weight;
         else
            for index in Sample_Weights.First_Index ..
              Sample_Weights.Last_Index loop
               Sample_Weights.Replace_Element
                 (index, Sample_Weights.Element (index) *
                      Expanded_Class_Weight.Element (index));
            end loop;
         end if;
      end if;

      --  L325
      if not Sample_Weights.Is_Empty then
         Sum_Sample_Weight := 0.0;
         for index in Sample_Weights.First_Index ..
           Sample_Weights.Last_Index loop
            Sum_Sample_Weight :=
              Sum_Sample_Weight + Sample_Weights.Element (index);
         end loop;
      end if;

      aClassifier.Attributes.Decision_Tree.Classes :=
        aClassifier.Attributes.Classes;

--        Printing.Print_Value_Data_Lists_2D
--          ("Base_Decision_Tree.Base_Fit, Classes",
--           aClassifier.Attributes.Decision_Tree.Classes);

      --  L410
      Ada_Tree_Builder.Build_Tree
        (aClassifier.Attributes.Decision_Tree, Splitter, Y_Encoded,
         aClassifier.Parameters.Max_Depth);

      if Integer (aClassifier.Attributes.Num_Outputs) = 1 then
         null;
      end if;

      Prune_Tree (aClassifier);

   end Base_Fit;

   --  -------------------------------------------------------------------------

   procedure Base_Fit_Checks
     (aClassifier    : in out Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y              : ML_Types.Value_Data_Lists_2D;
      Sample_Weights : in out Classifier_Types.Float_List) is
      use Maths.Float_Math_Functions;
      use Tree;
      Num_Samples           : constant Positive := Positive (X.Length);
      --  L226
      Max_Depth             : Natural;
      Max_Leaf_Nodes        : constant Integer := aClassifier.Parameters.Max_Leaf_Nodes;
      Min_Sample_Leaf       : constant Positive := 1;
      Min_Sample_Split      : Positive := 1;
      Max_Features          : Index_Range := Tree.Index_Range'Last;
      Sqrt_Num_Features     : Index_Range := 1;
   begin
      --  L226
      if aClassifier.Parameters.Max_Depth < 0 then
         Max_Depth := Integer'Last;
         aClassifier.Parameters.Max_Depth := Max_Depth;
      else
         Max_Depth := aClassifier.Parameters.Max_Depth;
      end if;

      --  L235
      if aClassifier.Parameters.Min_Samples_Leaf < 1 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Samples_Leaf must be at least 1";
      end if;

      --  L250
      if aClassifier.Parameters.Min_Samples_Split < 2 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Samples_Split must be at least 2";
      end if;
      Min_Sample_Split := aClassifier.Parameters.Min_Samples_Split;

      --  L263
      if Min_Sample_Split < 2 * Min_Sample_Leaf then
         Min_Sample_Split := 2 * Min_Sample_Leaf;
      end if;

      Sqrt_Num_Features :=
        Tree.Index_Range (Sqrt (Float (aClassifier.Attributes.Num_Features)));
      if Sqrt_Num_Features > 1 then
         Max_Features := Sqrt_Num_Features;
      else
         Max_Features := 1;
      end if;

      --  L291
      aClassifier.Parameters.Max_Features := Max_Features;

      if Positive (Y.Length) /= Num_Samples then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, number of labels " &
           Integer'Image (Integer (Y.Length)) &
           " does not match number of samples " & Integer'Image (Num_Samples);
      end if;

      --  L298
      if aClassifier.Parameters.Min_Weight_Fraction_Leaf < 0.0 or
        aClassifier.Parameters.Min_Weight_Fraction_Leaf > 5.0 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Weight_Fraction_Leaf is "
           & Float'Image (aClassifier.Parameters.Min_Weight_Fraction_Leaf) &
           " but should be in (0.0, 5.0]";
      end if;

      if Max_Features <= 0 or
        Max_Features > aClassifier.Attributes.Num_Features then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Max_Features is not in (0, Num_Features)";
      end if;

      --  L301
      if Max_Depth <= 0  then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, must be greater than 0.";
      end if;

      --  L305
      if Max_Leaf_Nodes /= -1 and Max_Leaf_Nodes < 2 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Max_Leaf_Nodes should be > 1";
      end if;
      --  L315
      if not Sample_Weights.Is_Empty then
         if Integer (Sample_Weights.Length) /= Num_Samples then
            raise Value_Error with
              "Base_Decision_Tree.Base_Fit_Checks, Sample_Weight lenghth " &
              "should be the same as the number of X samples";
         end if;
      end if;

      --  L350
      if aClassifier.Parameters.Min_Impurity_Decrease < 0.0 then
         raise Value_Error with
           "Base_Decision_Tree.Base_Fit_Checks, Min_Impurity_Decrease should be >= 0";
      end if;

   end Base_Fit_Checks;

   --  -------------------------------------------------------------------------

   procedure C_Init (aClassifier              : in out Classifier;
                     Criteria                 : Criterion.Criterion_Class;
                     Splitter                 : Node_Splitter.Splitter_Class;
                     Min_Samples_Split        : Positive := 2;
                     Min_Leaf_Samples         : Positive := 1;
                     Max_Features             : Tree.Index_Range :=
                       Tree.Index_Range'Last;
                     Class_Weight             : Weights.Weight_Type :=
                       Weights.No_Weight;
                     Max_Depth                : Integer := -1;
                     Min_Weight_Fraction_Leaf : Float := 0.0;
                     Max_Leaf_Nodes           : Integer := -1;
                     Min_Impurity_Decrease    : Float := 0.0;
                     CCP_Alpha                : Float := 0.0;
                     Random_State             : Integer := 0) is
   begin
      aClassifier.Parameters.Critera := Criteria;
      aClassifier.Parameters.Splitter := Splitter;
      aClassifier.Parameters.Max_Depth := Max_Depth;
      aClassifier.Parameters.Min_Samples_Split := Min_Samples_Split;
      aClassifier.Parameters.Min_Samples_Leaf := Min_Leaf_Samples;
      aClassifier.Parameters.Min_Weight_Fraction_Leaf :=
        Min_Weight_Fraction_Leaf;
      aClassifier.Parameters.Max_Features := Max_Features;
      aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
      aClassifier.Parameters.Min_Impurity_Decrease := Min_Impurity_Decrease;
      aClassifier.Parameters.Class_Weight := Class_Weight;
      aClassifier.Parameters.CCP_Alpha := CCP_Alpha;
      aClassifier.Parameters.Random_State := Random_State;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  based on L200 of _classes.py BasesDecisionTree.Fit
   procedure Classification_Part
     (aClassifier            : in out Classifier;
      Y                      : ML_Types.Value_Data_Lists_2D;
      Y_Encoded              : out Classifier_Types.List_Of_Natural_Lists;
      Classes                : out ML_Types.Value_Data_Lists_2D;
      Expanded_Class_Weights : in out Classifier_Types.Float_List) is
      use Ada.Containers;
      use Weights;
      Num_Outputs : constant Count_Type := Y.Element (1).Length;
      Y_Row       : ML_Types.Value_Data_List;
      Yk_Row      : ML_Types.Value_Data_List;
      YE_Row      : Natural_List;
      OP_Row      : ML_Types.Value_Data_List;
      Column      : Natural_List;
      Inverse     : Natural_List;
   begin
      aClassifier.Attributes.Classes.Clear;
      Y_Encoded.Clear;
      Classes.Clear;
      Y_Encoded.Set_Length (Y.Length);

      --  L208  Initialize Y_Encoded
      for row in Y.First_Index .. Y.Last_Index loop
         Column.Clear;
         for col in Y.Element (1).First_Index .. Y.Element (1).Last_Index loop
            Column.Append (col);
         end loop;
         Y_Encoded.Replace_Element (row, Column);
      end loop;

      OP_Row.Set_Length (Num_Outputs);
      for col in Y.Element (1).First_Index .. Y.Element (1).Last_Index loop
         Yk_Row.Clear;
         for row in Y.First_Index .. Y.Last_Index loop
            Y_Row := Y.Element (row);
            Yk_Row.Append (Y_Row.Element (col));
         end loop;
         Classes.Append (Encode_Utils.Unique (Yk_Row, Inverse));

         for row in Y.First_Index .. Y.Last_Index loop
            YE_Row := Y_Encoded.Element (row);
            YE_Row.Replace_Element (col, Inverse.Element (row));
            Y_Encoded.Replace_Element (row, YE_Row);
         end loop;

      end loop;
      aClassifier.Attributes.Classes := Classes;

      --  L218
      if aClassifier.Parameters.Class_Weight /= No_Weight then
         Expanded_Class_Weights :=
           Weights.Compute_Sample_Weight (No_Weight, Y);
      end if;

      Classes := aClassifier.Attributes.Classes;
      Printing.Print_Natural_Lists_2D ("Y_Encoded", Y_Encoded);
      Printing.Print_Value_Data_Lists_2D ("Classes", Classes);

   exception
      when others => raise Classifier_Error with
           "Base_Decision_Tree.Classification_Part error";

   end Classification_Part;

   --  -------------------------------------------------------------------------

   function Predict (Self : in out Classifier;
                     X    : ML_Types.Value_Data_Lists_2D)
                     return ML_Types.Value_Data_Lists_2D is
      use Ada.Containers;
      use ML_Types;
      use Weights;
      Num_Samples       : constant Count_Type := X.Length;
      Prob_A            : constant Weight_Lists_3D :=
                            Tree.Predict (Self.Attributes.Decision_Tree, X);
      Samples_K         : Weight_Lists_2D;
      Outputs_K         : Weight_List;
      Samples_2K        : Weight_List;
      Nodes_2K          : Weight_Lists_2D;
      Classes_K         : ML_Types.Value_Data_List;
      Selected_Classes  : ML_Types.Value_Data_List;
      Selected_Class    : ML_Types.Value_Record;
      Max_Indices       : Natural_List;
      --  Prediction 1 x num samples
      Pred              : ML_Types.Value_Data_List;
      --  Predictions, num samples x num outputs
      Predictions       : ML_Types.Value_Data_Lists_2D;
   begin
      Put_Line ("Base_Decision_Tree.Predict, Prob_A size: " &
                  Integer'Image (Integer (Prob_A.Length)) & " x " &
                  Integer'Image (Integer (Prob_A.Element (1).Length)));
      Printing.Print_Weight_Lists_3D ("Base_Decision_Tree.Predict, Prob_A",
                                      Prob_A);

      Predictions.Set_Length (Num_Samples);
      --  479
      for op in 1 .. Positive (Self.Attributes.Num_Outputs) loop
         Nodes_2K.Clear;
         for node_index in Prob_A.First_Index .. Prob_A.Last_Index loop
            Samples_K := Prob_A.Element (node_index);
            Samples_2K.Clear;
            for s_index in Samples_K.First_Index .. Samples_K.Last_Index loop
               Outputs_K := Samples_K.Element (s_index);
               Samples_2K.Append (Outputs_K.Element (op));
            end loop;
            Nodes_2K.Append (Samples_2K);
         end loop;

         --  for each node get the index of the sample with the highest value
         Max_Indices.Clear;
         for index in Nodes_2K.First_Index .. Nodes_2K.Last_Index loop
            --  Max_Indices: 1 x samples
            Samples_2K := Nodes_2K.Element (index);
            Max_Indices.Append (Max (Nodes_2K.Element (index)));
         end loop;
         Printing.Print_Weights ("Base_Decision_Tree.Predict, Samples_2K",
                                 Samples_2K);

         Classes_K := Self.Attributes.Classes.Element (op);
         Selected_Classes.Clear;
         for index in Max_Indices.First_Index .. Max_Indices.Last_Index loop
            Selected_Class := Classes_K.Element (Max_Indices (index));
            Selected_Classes.Append (Selected_Class);
         end loop;
         Printing.Print_Value_Data_List ("Base_Decision_Tree.Predict, Selected_Classes",
                                         Selected_Classes);
         Pred.Clear;
         for index in Selected_Classes.First_Index ..
           Selected_Classes.Last_Index loop
            Pred.Append (Selected_Classes.Element (index));
         end loop;
         Predictions.Replace_Element (op, Pred);
      end loop;

      return Predictions;

   end Predict;

   --  -------------------------------------------------------------------------
   --  Prune tree using Minimal Cost-Complexity Pruning.
   procedure Prune_Tree (aClassifier : in out Classifier) is
   begin
      null;
   end Prune_Tree;

   --  -------------------------------------------------------------------------

end Base_Decision_Tree;
