
with ML_Types;
with Classifier_Types;
with Weights;

package Criterion is

   type Criterion_Class is record
      Y                         : Classifier_Types.List_Of_Natural_Lists;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight             : Weights.Weight_List;
      Samples                   : ML_Types.List_Of_Value_Data_Lists;
      Start                     : Natural := 0;
      Stop                      : Natural := 0;
      Position                  : Natural := 0;
      Num_Node_Samples          : Natural := 0;
      Num_Weighted_Samples      : Float := 0.0;
      Num_Weighted_Node_Samples : Float := 0.0;
      Num_Weighted_Left         : Float := 0.0;
      Num_Weighted_Right        : Float := 0.0;
--        Weighted_Node_Samples     : Weights.Weight_List;
      --  For classification criteria, Sum_Total is the sum of the weighted
      --  count of each class.
      --  For regression, Sum_Total is the sum of w*y.
      --  Sum_Total [k] is equal to
      --  sum_{i=start}^{end-1} w[samples[i]]*y[samples[i], k]
      --  where k is the output index.
      Sum_Total                 : Weights.Weight_Lists_List;
      Sum_Left                  : Weights.Weight_Lists_List;
      Sum_Right                 : Weights.Weight_Lists_List;
      Sq_Sum_Total              : Float := 0.0;
      --  Classification Criterion_Class
      Classes                   : ML_Types.List_Of_Value_Data_Lists;
      Sample_Indices            : Classifier_Types.Natural_List;
      Proxy_Improvement         : Float := -Float'Last;
   end record;

   Criterion_Error : Exception;

   procedure Classification_Init
     (Criteria         : in out Criterion_Class;
      Y                : Classifier_Types.List_Of_Natural_Lists;
      --  Samples:
      Sample_Indices   : Classifier_Types.Natural_List;
      --  Sample_Weight contains the weight of each sample
      Sample_Weight    : Weights.Weight_List;
      Weighted_Samples : Float;
      Start, Stop      : Natural);
   procedure Gini_Children_Impurity (Criteria       : Criterion_Class;
                                     Impurity_Left,
                                     Impurity_Right : out Float);
   function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float;
   function Impurity_Improvement
     (Criteria                                       : Criterion_Class;
      Impurity_Parent, Impurity_Left, Impurity_Right : Float) return Float;
   procedure Init (Criteria : in out Criterion_Class;
                   Classes  : ML_Types.List_Of_Value_Data_Lists);
   function Entropy_Node_Impurity (Self : Criterion_Class) return Float;
   procedure Node_Value (Self  : Criterion_Class;
                         Value : out Classifier_Types.List_Of_Float_Lists);
   function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                         return Float;
   procedure Reset (Criteria : in out Criterion_Class);
   procedure Update (Criteria : in out Criterion_Class;
                     New_Pos  : Positive);

end Criterion;
