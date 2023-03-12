
with ML_Types;
with Tree;

package Tree_Classifier_Utilities is

   type Feature_Type_Array is array (Positive range <>) of ML_Types.Data_Type;
   type Label_Type_Array is array (Positive range <>) of ML_Types.Data_Type;

   Value_Error : exception;

   function Count_Samples (aClassifier : Base_Decision_Tree.Classifier)
                            return Natural;
   function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                            return Tree.Tree_Cursor;

end Tree_Classifier_Utilities;
