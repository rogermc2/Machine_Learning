
with Ada.Containers.Doubly_Linked_Lists;

with Tree;

package Priority_Heap is

   type Priority_Record is record
      Node           : Tree.Tree_Node;
      Depth          : Natural;
      Is_Leaf        : Boolean := False;
      Impurity       : Float;
      Impurity_Left  : Float;
      Impurity_Right : Float;
      Improvement    : Float;
      Start          : Positive := 1;
      Stop           : Positive := 1;
      Position       : Positive := 1;
   end record;

   package Frontier_Package is new
     Ada.Containers.Doubly_Linked_Lists (Priority_Record);
   subtype Frontier_List is Frontier_Package.List;
   subtype Frontier_Cursor is Frontier_Package.Cursor;

end Priority_Heap;
