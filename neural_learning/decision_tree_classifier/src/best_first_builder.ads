
with Tree;
with Tree_Build;

package Best_First_Builder is

   Best_First_Build_Error : Exception;

   procedure Build_Tree (Builder : in out Tree_Build.Tree_Builder;
                         theTree : in out Tree.Tree_Class);

end Best_First_Builder;
