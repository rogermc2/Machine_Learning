
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Neural_Utilities;
with Python_CLF;

package body Support_12A is

   Lex_Size    : constant Unbounded_String := To_Unbounded_String ("@size");
   Unknown     : constant Unbounded_String := To_Unbounded_String ("@unk");
   Num_Known   : Natural := 0;
   Num_Unknown : Natural := 0;

   function Tokenize (Data : String; Dictionary : Dictionary_List)
                      return Integer_Array;

   --  -------------------------------------------------------------------------

   function Arg_Max (Indices : Integer_Array; Values : Integer_Matrix)
                     return Positive is
      Routine_Name : constant String := "Support_6A.Arg_Max ";
      Data         : constant Integer_Array := Get_Row (Values, 1);
      Best         : constant Integer := Max (Data);
      Found        : Boolean := False;
      index        : Integer := Indices'First - 1;
      Result       : Positive := Indices (Indices'First);
   begin
      Print_Matrix_Dimensions (Routine_Name & "Values", Values);
      while index <= Indices'last and not Found loop
         index := index + 1;
         Found := Data (index) = Best;
         if Found then
            Result := Indices (index);
         end if;
      end loop;

      return Result;

   end Arg_Max;

   --  -------------------------------------------------------------------------

   function Find_Item
     (Dictionary : Dictionary_List; Key : Unbounded_String;
      Item       : out Dictionary_Record) return Boolean is
      use Dictionary_Package;
      --        Routine_Name : constant String := "Support_6A.Find_Item ";
      Curs  : Cursor := Dictionary.First;
      Found : Boolean := False;
   begin
      while Has_Element (Curs) and not Found loop
         Item := Element (Curs);
         Found := Item.Key = Key;
         Next (Curs);
      end loop;

      return Found;

   end Find_Item;
   pragma Inline (Find_Item);

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String; Dictionary : Dictionary_List)
                      return Data_Record is
      Routine_Name    : constant String := "Support_12A.Get_Data ";
      File_ID         : File_Type;
      Data            : Data_Record;
   begin
      Open (File_ID, In_File, File_Name);

      while not End_Of_File (File_ID) loop
         declare
            aLine : constant String := Get_Line (File_ID);
            Label : constant Integer := Integer'Value (aLine (1 .. 1));
            Token : constant Integer_Array :=
                      Tokenize (aLine (3 .. aLine'Last), Dictionary);
         begin
            Data.Labels.Append (Label);
            Data.Features.Append (Token);
         end;
      end loop;

      Close (File_ID);
      Put_Line (Routine_Name & File_Name & " procesed.");

      return Data;

   end Get_Data;

   --  -------------------------------------------------------------------------
   --  For alpha days the selections are random.
   function Play_Game (Classifier   : Python.Module; Rounds : Positive;
                       Data, Labels : Integer_Array;
                       Alpha        : Integer; Chooser : Chooser_Access)
                       return ML_Types.Integer_List is
      use ML_Types;
      --        Routine_Name : constant String := "Support_12A.Play_Game ";
      B            : constant Positive := 5;
      Clf          : constant Python_API.PyObject :=
                       Python.Call (Classifier, "multinomial_nb");
--        Data_List    : Integer_List;
--        Labels_List  : Integer_List;
      Train_Set    : Integer_List_2D;
      Train_Labels : Integer_List_2D;
      current_item : Positive := 1;
      Item         : Integer;
      Train_Item   : Integer_List;
      Labels_Item  : Integer_List;
      Score        : Integer_List;
   begin
      while current_item < Rounds loop
         Train_Item.Clear;
         Labels_Item.Clear;
         Item := Chooser (Classifier, current_item, B, Train_Set,
                          Train_Labels, Alpha, Clf);
         Score.Append (Labels (Item));
         Train_Item.Append (Data (Item));
         Labels_Item.Append (Labels (Item));
         Train_Set.Append (Train_Item);
         Train_Set.Append (Labels_Item);
         current_item := current_item + B;
      end loop;

      return Score;

   end Play_Game;

   --  -------------------------------------------------------------------------
   --  ProbA_Chooser chooses between B options.
   --  Current_Item is the initial item to consider.
   --  Train_Set represents the results of previous selections.
   --  If alpha selections have not yet made the selection is random.
   --  If alpha selections have been made in the past, fit a clf Naive Bayes
   --  model using the traing data of academic papers by title, trainset and
   --  training labels if the academic papers were interesting, trainlabs.
   --  After fitting the clf model use it to select the item most likely to be
   --  labeled as interesting.
   function ProbA_Chooser
     (Classifier   : Python.Module; Current_Item : Positive;
      B            : Positive;
      Train_Set    : in out ML_Types.Integer_List_2D;
      Train_Labels : in out ML_Types.Integer_List_2D;
      Alpha        : Integer;  Clf : Python_API.PyObject) return Integer is
      Routine_Name : constant String := "Support_12.ProbA_Chooser ";
      Indices      : Integer_Array (1 .. B);
      --  Y_Hat predictions
      Y_Hat        : Integer_Matrix (Train_Set.First_Index ..
                                       Train_Set.Last_Index, 1 .. 1);
      Item         : Integer;
   begin
      Put_Line (Routine_Name & "Train_Set length " &
                  Integer'Image (Integer (Train_Set.Length)));
      if Integer (Train_Set.Length) = Alpha then
         Put_Line (Routine_Name & "Train_Labels length " &
                     Integer'Image (Integer (Train_Labels.Length)));
         Put_Line (Routine_Name & "fitting Train_Set and Train_Labels.");
         if Integer (Train_Labels.Length) > 0 then
            Python_CLF.Call (Classifier, "fit", Clf, Train_Set, Train_Labels);
         else
            Put_Line (Routine_Name & "fitting Train_Set.");
            Python_CLF.Call (Classifier, "fit", Clf, Train_Set);
         end if;
         Put_Line (Routine_Name & "fitted.");
      end if;

      Put_Line (Routine_Name & "Train_Set length " &
                  Integer'Image (Integer (Train_Set.Length)));
      if Integer (Train_Set.Length) < Alpha then
         Item := Maths.Random_Integer (Current_Item, Current_Item + B);
      else
         Put_Line (Routine_Name & "Train_Set (1) length " &
                     Integer'Image (Integer (Train_Set (1).Length)));
         --  predict_proba() method returns a two-dimensional array,
         --  containing the estimated probabilities for each instance and each
         --  class:
         Y_Hat := Python_CLF.Call (Classifier, "predict_proba", Clf, Train_Set);
         Print_Matrix_Dimensions (Routine_Name & "Y_Hat", Y_Hat);
         for index in Indices'Range loop
            Indices (index) := Current_Item + index - 1;
         end loop;
         Item := Arg_Max (Indices, Y_Hat) - 1;
      end if;

      return Item;

   end ProbA_Chooser;

   --  -------------------------------------------------------------------------

   function Read_Vocabulary (File_Name : String) return Dictionary_List is
      Routine_Name     : constant String := "Support_12A.Read_Vocabulary ";
      File_ID          : File_Type;
      Lexicon_Size     : Natural := 0;  --  Token
      Vocab_Dictionary : Dictionary_List;
      Item             : Dictionary_Record;
   begin
      Item := (Unknown, Lexicon_Size);
      Vocab_Dictionary.Append (Item);
      Lexicon_Size := Lexicon_Size + 1;

      Open (File_ID, In_File, File_Name);

      while not End_Of_File (File_ID) loop
         declare
            aLine : constant Unbounded_String :=
                      To_Unbounded_String (Get_Line (File_ID));
            Count : constant Positive := Integer'Value (Slice (aLine, 1, 4));
            Token : constant Unbounded_String :=
                      To_Unbounded_String
                        (Slice (aLine, 6, Length (aLine) - 1));
         begin
            if Count > 1 then
               Item :=  (Token, Lexicon_Size);
               Vocab_Dictionary.Append (Item);
               Lexicon_Size := Lexicon_Size + 1;
            end if;
         end;
      end loop;

      Close (File_ID);

      Item :=  (Lex_Size, Lexicon_Size);
      Vocab_Dictionary.Append (Item);
      Put_Line (Routine_Name & File_Name & " procesed.");

      return Vocab_Dictionary;

   end Read_Vocabulary;

   --  -------------------------------------------------------------------------

   function To_Integer_Array (A : Integer_Array_List) return Integer_Array is
      --        Routine_Name : constant String := "Support_12A.To_Integer_Array ";
      Result : constant Integer_Array := A (1);
   begin

      return Result;

   end To_Integer_Array;

   --  -------------------------------------------------------------------------

   function Tokenize (Data : String; Dictionary : Dictionary_List)
                      return Integer_Array is
      use Neural_Utilities;
      use ML_Types;
      use String_Package;
      --        Routine_Name : constant String := "Support_6A.Tokenize ";
      Words        : ML_Types.String_List;
      Word_Cursor  : String_Package.Cursor;
      Index        : Natural;
      Item         : Dictionary_Record;
      Vec          : Integer_Array (0 .. Positive (Dictionary.Length) - 1) :=
                       (others => 0);
      Word         : Unbounded_String;
      Dummy        : Boolean;
   begin
      Words := Split_String_On_Spaces (Data);
      Word_Cursor := Words.First;
      while Has_Element (Word_Cursor) loop
         Word := Element (Word_Cursor);
         if Find_Item (Dictionary, Word, Item) then
            Num_Known := Num_Known + 1;
            Index := Item.Value;
         else
            Num_Unknown := Num_Unknown + 1;
            Dummy := Find_Item (Dictionary, Unknown, Item);
            Index := Item.Value;
         end if;

         Vec (Index) := Vec (Index) + 1;
         Next  (Word_Cursor);
      end loop;

      return Vec;

   end Tokenize;

   --  -------------------------------------------------------------------------

   --     function Word_List  (Dictionary : Dictionary_List)
   --                          return ML_Types.Indef_String_List is
   --        use ML_Types;
   --        use Dictionary_Package;
   --        --        Routine_Name : constant String := "Support_6A.Word_List ";
   --        Curs  : Cursor := Dictionary.First;
   --        Words : Indef_String_List;
   --        Item  : Dictionary_Record;
   --     begin
   --        while Has_Element (Curs) loop
   --           if Curs /= Dictionary.Last then
   --              Item := Element (Curs);
   --              Words.Append (To_String (Item.Key));
   --           end if;
   --           Next (Curs);
   --        end loop;
   --
   --        return Words;
   --
   --     end Word_List;

   --  -------------------------------------------------------------------------

end Support_12A;
