
package Priority_Heap is

   type Priority_Heap_Record is record
      Depth          : Natural;
      Is_Leaf        : Boolean := False;
      Impurity       : Float;
      Impurity_Left  : Float;
      Impurity_Right : Float;
      Improvement    : Float;
   end record;

end Priority_Heap;
