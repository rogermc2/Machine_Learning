
with IL_Types; use IL_Types;
--  with Tree;
with Weights;

package Criterion is

   type Criterion_Kind is (Criterion_Classification, Criterion_Regression);
   type Classifier_Criteria_Type is (Gini_Criteria, Entropy_Criteria);
   type Regressor_Criteria_Type is (MSE_Criteria, Friedman_MSE_Criteria,
                                    MAE_Criteria);
   type Criterion_Class
     (Criterion_Type : Criterion_Kind := Criterion_Classification) is record
      Y                         : Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight             : Weights.Weight_List;
      Samples                   : IL_Types.Float_List_2D;
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
      Sum_Total                 : Float_List;
      Sum_Left                  : Float_List;
      Sum_Right                 : Float_List;
      Sample_Indices            : Natural_List;
      Proxy_Improvement         : Float := -Float'Last;
      case Criterion_Type is
         when Criterion_Classification =>
            Num_Classes  : Natural;
         when Criterion_Regression =>
            Sq_Sum_Total : Float := 0.0;
      end case;
   end record;

   Criterion_Error : Exception;

   procedure C_Init (Criteria    : in out Criterion_Class;
                     Num_Classes : Natural);
   procedure Children_Impurity_Gini (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float);
   function Impurity_Improvement
     (Criteria                                       : Criterion_Class;
      Impurity_Parent, Impurity_Left, Impurity_Right : Float) return Float;
   procedure Initialize_Node_Criterion
      (Criteria           : in out Criterion_Class;
      Y_Encoded           : Natural_List;
      --  Samples:
      Sample_Indices      : Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight       : Weights.Weight_List;
      Weighted_Samples    : Float;
      Start_Row, Stop_Row : Natural);
   function Node_Impurity_Entropy (Self : Criterion_Class) return Float;
   function Node_Impurity_Gini (Criteria : Criterion_Class) return Float;
   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Weights.Weight_List);
--                           Value : out Weights.Weight_Lists_2D);
   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                         return Float;
   procedure Reset (Criteria : in out Criterion_Class);
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive);

end Criterion;
