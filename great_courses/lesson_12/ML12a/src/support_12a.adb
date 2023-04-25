
with System;

with Ada.Assertions; use Ada.Assertions;
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
   --     function To_Matrix (A : Integer_Array_List) return Integer_Matrix;

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
                      return Data_Items is
      Routine_Name : constant String := "Support_12A.Get_Data ";
      File_ID      : File_Type;
      Num_Lines    : Natural := 0;
   begin
      Put_Line (Routine_Name & "processing " & File_Name);
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         Num_Lines := Num_Lines + 1;
         Skip_Line (File_ID);
      end loop;

      Reset (File_ID);
      Put_Line (Routine_Name & "Num Lines: " & Integer'Image (Num_Lines));
      declare
         Data : Data_Items (Num_Lines);
         Row  : Natural := 0;
      begin
         Put_Line (Routine_Name & "reading " & File_Name);
         Num_Lines := 0;
         Put ("*");

         while not End_Of_File (File_ID) loop
            Row := Row + 1;
            Num_Lines := Num_Lines + 1;
            if Num_Lines mod 100 = 0 then
               Put ("*");
            end if;

            declare
               aLine : constant String := Get_Line (File_ID);
               --  Token arrays are of varying length
               Token : constant Integer_Array :=
                         Tokenize (aLine (3 .. aLine'Last), Dictionary);
            begin
               Data.Labels (Row, 1) := Integer'Value (aLine (1 .. 1));
               Data.Features.Append (Token);
            end;

         end loop;
         New_Line;

         Close (File_ID);

         Put_Line (Routine_Name & File_Name & Integer'Image (Num_Lines) &
                     " lines processed.");
         return Data;
      end;

   end Get_Data;

   --  -------------------------------------------------------------------------
   --  For alpha days the selections are random.
   function Play_Game (Classifier : Python.Module; Rounds : Positive;
                       Data       : Data_Items; Alpha : Integer)
                       return ML_Types.Integer_List is
      use System;
      use ML_Types;
      Routine_Name : constant String := "Support_12A.Play_Game ";
      B            : constant Positive := 5;
      Clf          : Python_API.PyObject;
      Train_Set    : Integer_List_2D;
      Train_Labels : Integer_List;
      current_item : Positive := 1;
      Item         : Integer;
      Score        : Integer_List;
   begin
      Put_Line (Routine_Name);
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Put_Line (Routine_Name & "init_MultinomialNB");
      Clf := Python.Call (Classifier, "init_MultinomialNB");
      Print_Matrix_Dimensions (Routine_Name & "Labels", Data.Labels);

      while current_item < Rounds loop
         Item := ProbA_Chooser (Classifier, current_item, B, Train_Set,
                                Train_Labels, Alpha, Clf);
         declare
            Features   : constant Integer_Array := Data.Features (Item);
            Train_Item : Integer_List;
         begin
            for col in Features'Range loop
               Train_Item.Append (Features (col));
            end loop;
            Train_Labels.Append (Train_Item);
         end;

         Train_Labels.Append (Data.Labels (Item, 1));
         Score.Append (Data.Labels (Item, 1));

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
      Train_Labels : in out ML_Types.Integer_List;
      Alpha        : Integer;  Clf : Python_API.PyObject) return Integer is
      use System;
      Routine_Name : constant String := "Support_12.ProbA_Chooser ";
      Indices      : Integer_Array (1 .. B);
      --  Y_Hat predictions
      Y_Hat        : Integer_Matrix (Train_Set.First_Index ..
                                       Train_Set.Last_Index, 1 .. 1);
      Item         : Integer;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Put_Line (Routine_Name & "Train_Set length " &
                  Integer'Image (Integer (Train_Set.Length)));
      if Integer (Train_Set.Length) = Alpha then
         Put_Line (Routine_Name & "Train_Labels length " &
                     Integer'Image (Integer (Train_Labels.Length)));
         Put_Line (Routine_Name & "fitting Train_Set and Train_Labels.");
         Python_CLF.Call (Classifier, "fit", Clf, Train_Set, Train_Labels);
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
      Put_Line (Routine_Name & File_Name & " processed.");

      return Vocab_Dictionary;

   end Read_Vocabulary;

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
      Unknown_Item : constant Boolean := Find_Item (Dictionary, Unknown, Item);
      Unknown_Val  : constant Integer := Item.Value;
      Vec          : Integer_Array (0 .. Positive (Dictionary.Length) - 1) :=
                       (others => 0);
      Word         : Unbounded_String;
   begin
      pragma Warnings (Off, Unknown_Item);
      Words := Split_String_On_Spaces (Data);
      Word_Cursor := Words.First;
      while Has_Element (Word_Cursor) loop
         Word := Element (Word_Cursor);
         if Find_Item (Dictionary, Word, Item) then
            --  the word has a feature so add one to the corresponding feature
            Num_Known := Num_Known + 1;
            Index := Item.Value;
         else
            --  add one to the Unknown
            Num_Unknown := Num_Unknown + 1;
--              Dummy := Find_Item (Dictionary, Unknown, Item);
            Index := Unknown_Val;
         end if;

         Vec (Index) := Vec (Index) + 1;
         Next  (Word_Cursor);
      end loop;

      return Vec;

   end Tokenize;

   --  -------------------------------------------------------------------------

end Support_12A;
