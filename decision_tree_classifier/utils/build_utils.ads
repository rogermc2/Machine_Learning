
with Ada.Containers.Doubly_Linked_Lists;

with Tree;

package Build_Utils is

   type Priority_Record is record
      Node_Cursor    : Tree.Tree_Cursor;
      Node_Params    : Tree.Tree_Node;
      Depth          : Natural;
      Is_Leaf        : Boolean := False;
      Impurity       : Float;
      Impurity_Left  : Float;
      Impurity_Right : Float;
      Improvement    : Float;
      Start          : Positive := 1;
      Stop_Row       : Positive := 1;
      Position       : Positive := 1;
   end record;

   type Stack_Record is record
      Parent_Cursor         : Tree.Tree_Cursor;
      Start                 : Positive := 1;
      Stop                  : Positive := 1;
      Depth                 : Natural := 0;
      Is_Left               : Boolean := True;
      Impurity              : Float := Float'Last;
      Num_Constant_Features : Natural := 0;
   end record;

   package Frontier_Package is new
     Ada.Containers.Doubly_Linked_Lists (Priority_Record);
   subtype Frontier_List is Frontier_Package.List;
   subtype Frontier_Cursor is Frontier_Package.Cursor;

   package Stack_Package is new
     Ada.Containers.Doubly_Linked_Lists (Stack_Record);
   subtype Stack_List is Stack_Package.List;
   subtype Stack_Cursor is Stack_Package.Cursor;

   function Pop (aStack : in out Stack_List) return Stack_Record;
   procedure Push (aStack                : in out Stack_List;
                   Start, Stop           : Positive;
                   Depth                 : Natural;
                   Parent                : Tree.Tree_Cursor;
                   Is_Left               : Boolean;
                   Impurity              : Float;
                   Num_Constant_Features : Natural);

end Build_Utils;
