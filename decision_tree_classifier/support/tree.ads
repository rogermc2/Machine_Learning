
--  Based on Python 3.7 sklearn tree _tree.pxd class Tree

--  and, sometimes, code sections which are only meant for inclusion by Cython modules.
--  The tree structure is used for predictions and feature importances.

package Tree is

   type Tree_Data is record
      Num_Features : Integer := 0;
      Num_Classes  : Integer := 0;
      Num_Outputs  : Integer := 0;
      Max_Classes  : Integer := 0;
   end record;

end Tree;
