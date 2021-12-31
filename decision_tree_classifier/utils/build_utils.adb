
package body Build_Utils is

   function Pop (aStack : in out Frontier_List) return Priority_Record is
      Data : Priority_Record;
   begin
      Data := aStack.First_Element;
      aStack.Delete_First;
      return Data;
   end Pop;

   --  ------------------------------------------------------------------

   function Pop (aStack : in out Stack_List) return Stack_Record is
      Data : Stack_Record;
   begin
      Data := aStack.First_Element;
      aStack.Delete_First;
      return Data;
   end Pop;

   --  ------------------------------------------------------------------

   procedure Push (aStack : in out Frontier_List;
                   Data   : Priority_Record) is
   begin
      aStack.Prepend (Data);
   end Push;

   --  ------------------------------------------------------------------

   procedure Push (aStack                : in out Frontier_List;
                   Is_Leaf               : Boolean;
                   Start, Stop, Position : Positive;
                   Depth                 : Natural;
                   Parent                : Tree.Tree_Cursor;
                   Impurity              : Float;
                   Improvement           : Float) is
      Data : Priority_Record;
   begin
      Data.Node_Cursor := Parent;
      Data.Depth := Depth;
      Data.Is_Leaf := Is_Leaf;
      Data.Impurity := Impurity;
      Data.Improvement := Improvement;
      Data.Start := Start;
      Data.Stop_Row := Stop;
      Data.Position := Position;
      aStack.Prepend (Data);
   end Push;

   --  ------------------------------------------------------------------

   procedure Push (aStack                : in out Stack_List;
                   Start, Stop           : Positive;
                   Depth                 : Natural;
                   Parent                : Tree.Tree_Cursor;
                   Branch                : Tree.Node_Type;
                   Impurity              : Float;
                   Num_Constant_Features : Natural) is
      Data : Stack_Record;
   begin
      Data.Parent_Cursor := Parent;
      Data.Start := Start;
      Data.Stop  := Stop;
      Data.Depth := Depth;
      Data.Branch := Branch;
      Data.Impurity := Impurity;
      Data.Num_Constant_Features := Num_Constant_Features;
      aStack.Prepend (Data);
   end Push;

   --  ------------------------------------------------------------------

end Build_Utils;
