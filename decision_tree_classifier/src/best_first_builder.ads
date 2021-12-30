
with Classifier_Types;
with ML_Types;
with Tree;
with Tree_Build;
with Weights;

package Best_First_Builder is

   Best_First_Build_Error : Exception;

   procedure Build_Tree (Builder       : in out Tree_Build.Tree_Builder;
                         theTree       : in out Tree.Tree_Class;
                         X             : ML_Types.Value_Data_Lists_2D;
                         Y_Encoded     : Classifier_Types.Natural_Lists_2D;
                         Sample_Weight : Weights.Weight_List);

end Best_First_Builder;
