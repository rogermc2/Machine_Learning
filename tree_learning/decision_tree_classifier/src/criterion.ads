
with ML_Types;
with NL_Types;
with Tree;
with Weights;

package Criterion is

   type Criterion_Kind is (Criterion_Classification, Criterion_Regression);
   type Classifier_Criteria_Type is (Gini_Criteria, Entropy_Criteria);
   type Regressor_Criteria_Type is (MSE_Criteria, Friedman_MSE_Criteria,
                                    MAE_Criteria);
   type Criterion_Class
     (Criterion_Type : Criterion_Kind := Criterion_Classification) is record
      Y                         : NL_Types.Natural_Lists_2D;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight             : Weights.Weight_List;
      Samples                   : ML_Types.Value_Data_Lists_2D;
      Num_Outputs               : Tree.Index_Range := 1;
      Start_Row                 : Natural := 0;
      Stop_Row                  : Natural := 0;
      Split_Row                 : Natural := 0;
      Num_Node_Samples          : Natural := 0;
      Num_Weighted_Samples      : Float := 0.0;
      Num_Weighted_Node_Samples : Float := 0.0;
      Num_Weighted_Left         : Float := 0.0;
      Num_Weighted_Right        : Float := 0.0;
      --  For classification criteria, Sum_Total is the sum of the
      --   weighted count of each class.
      --  For regression, Sum_Total is the sum of w*y.
      --  Sum_Total [k] is equal to
      --  sum_{i=start}^{end-1} w[samples[i]]*y[samples[i], k]
      --  where k is the output index.
      Sum_Total                 : Weights.Weight_Lists_2D;
      Sum_Left                  : Weights.Weight_Lists_2D;
      Sum_Right                 : Weights.Weight_Lists_2D;
      Sample_Indices            : NL_Types.Natural_List;
      Proxy_Improvement         : Float := -Float'Last;
      case Criterion_Type is
         when Criterion_Classification =>
            --  each output's number of classes for
            Num_Classes  : NL_Types.Natural_List;
         when Criterion_Regression =>
            Sq_Sum_Total : Float := 0.0;
      end case;
   end record;

   Criterion_Error : Exception;

   procedure C_Init (Criteria    : in out Criterion_Class;
                     Num_Outputs : Tree.Index_Range := 1;
                     Num_Classes : NL_Types.Natural_List :=
                       NL_Types.Natural_Package.Empty_Vector);
   procedure Children_Impurity_Gini (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float);
   function Impurity_Improvement
     (Criteria                                       : Criterion_Class;
      Impurity_Parent, Impurity_Left, Impurity_Right : Float) return Float;
   procedure Initialize_Node_Criterion
      (Criteria           : in out Criterion_Class;
      Y                   : NL_Types.Natural_Lists_2D;
      --  Samples:
      Sample_Indices      : NL_Types.Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight       : Weights.Weight_List;
      Weighted_Samples    : Float;
      Start_Row, Stop_Row : Natural);
   function Node_Impurity_Entropy (Self : Criterion_Class) return Float;
   function Node_Impurity_Gini (Criteria : Criterion_Class) return Float;
   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Weights.Weight_Lists_2D);
   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                         return Float;
   procedure Reset (Criteria : in out Criterion_Class);
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive);

end Criterion;
