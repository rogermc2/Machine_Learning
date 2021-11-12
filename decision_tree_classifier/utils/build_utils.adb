
package body Build_Utils is

   function Pop (aStack : in out Stack_List) return Stack_Record is
      Data : Stack_Record;
   begin
      Data := aStack.First_Element;
      aStack.Delete_First;
      return Data;
   end Pop;

   procedure Push (aStack                : in out Stack_List;
                   Start, Stop           : Positive;
                   Depth                 : Natural;
                   Parent                : Tree.Tree_Cursor;
                   Is_Left               : Boolean;
                   Impurity              : Float;
                   Num_Constant_Features : Natural) is
      Data : Stack_Record;
   begin
      Data.Parent_Cursor := Parent;
      Data.Start := Start;
      Data.Stop  := Stop;
      Data.Depth := Depth;
      Data.Is_Left := Is_Left;
      Data.Impurity := Impurity;
      Data.Num_Constant_Features := Num_Constant_Features;
      aStack.Prepend (Data);
   end Push;

end Build_Utils;
