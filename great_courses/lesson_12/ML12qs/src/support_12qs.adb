
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Neural_Utilities;
with Python_API;
with Python_CLF;

package body Support_12QS is

   Lex_Size    : constant Unbounded_String := To_Unbounded_String ("@size");
   Unknown     : constant Unbounded_String := To_Unbounded_String ("@unk");
   Num_Known   : Natural := 0;
   Num_Unknown : Natural := 0;

   function Tokenize (aLine : String; Dictionary : Dictionary_List)
                      return Integer_Array;
   pragma Inline (Tokenize);

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the index from indices associated with the item in the
   --  Values list with the highest value.
--     function Arg_Max (Indices : Integer_Array; Values : Real_Float_Vector)
--                       return Integer is
--        --        Routine_Name : constant String := "Support_12QS.Arg_Max ";
--        Best_Index   : Positive;
--        pragma Warnings (Off);
--        Best         : constant Float := Max (Values, Best_Index);
--        pragma Warnings (On);
--     begin
--        return Indices (Best_Index);
--
--     end Arg_Max;
--     pragma Inline (Arg_Max);

   --  -------------------------------------------------------------------------

   function Find_Item
     (Dictionary : Dictionary_List; Key : Unbounded_String;
      Item       : out Dictionary_Record) return Boolean is
      use Dictionary_Package;
      --        Routine_Name : constant String := "Support_12QS.Find_Item ";
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
      Routine_Name : constant String := "Support_12QS.Get_Data ";
      File_ID      : File_Type;
      Num_Lines    : Natural := 0;
   begin
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         Num_Lines := Num_Lines + 1;
         Skip_Line (File_ID);
      end loop;

      Reset (File_ID);
      Put_Line (Routine_Name & "processing " & File_Name & ", Num Lines:" &
                  Integer'Image (Num_Lines));
      declare
         Data : Data_Items (Num_Lines);
         Row  : Natural := 0;
      begin
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
               --                 Delay (3.0);
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

   function Play_Game (Classifier     : Python.Module; Rounds : Positive;
                       Labeled_Titles : Data_Items; Alpha : Float)
                       return Natural is
      use ML_Types;
--        Routine_Name : constant String := "Support_12QS.Play_Game ";
      Num_Titles    : constant Positive := 5;  -- b
      Train_Set     : Integer_List_2D;
      Train_Labels  : Integer_List;
      Current_Title : Positive := 1;
      Title_ID      : Integer;
      Chosen_Label  : Natural;
      Score         : Natural := 0;
      Count         : Natural := 0;
   begin
      while Current_Title < Rounds loop
         if Count mod 10 = 0 then
            Put ("*");
         end if;
         Count := Count + 1;

         Title_ID := Thomson_Chooser
           (Classifier, Current_Title, Num_Titles, Labeled_Titles, Train_Set,
            Train_Labels, Alpha);
         Chosen_Label := Labeled_Titles.Labels (Title_ID, 1);
         Score := Score + Chosen_Label;
         Train_Labels.Append (Chosen_Label);

         declare
            Features    : constant Integer_Array :=
                            Labeled_Titles.Features (Title_ID);
            Train_Title : Integer_List;
         begin
            for col in Features'Range loop
               Train_Title.Append (Features (col));
            end loop;

            --  Maximum length of Train_Set is Rounds / B
            Train_Set.Append (Train_Title);
         end;

         Current_Title := Current_Title + Num_Titles;
      end loop;
      New_Line;

      return Score;

   end Play_Game;

   --  -------------------------------------------------------------------------

   function Pick_One (V : Integer_Array) return Positive is
      C    : Natural := 0;
      Pick : Positive;
   begin
      Pick := Maths.Random_Integer (1, V'Length);
      for index in V'Range loop
         if V (index) = 1 then
            C := C + 1;
         end if;
      end loop;

      return Pick;

   end Pick_One;

   --  -------------------------------------------------------------------------

   function Read_Vocabulary (File_Name : String) return Dictionary_List is
      Routine_Name     : constant String := "Support_12QS.Read_Vocabulary ";
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

   function Thomson_Chooser
     (Classifier     : Python.Module;
      Current_Item   : Positive;
      Num_Titles     : Positive;  --  b
      Labeled_Titles : Data_Items;
      Train_Set      : ML_Types.Integer_List_2D;
      Train_Labels   : ML_Types.Integer_List;
      Alpha          : Float) return Integer is
      use System;
      Routine_Name     : constant String := "Support_12QS.Thomson_Chooser ";
      Clf              : Python_API.PyObject;
--        Train_Set_Length : constant Natural := Natural (Train_Set.Length);
      NTM1             : constant Natural := Num_Titles - 1;
      Title_ID         : Integer;
   begin
      --  Maximum length of Train_Set is Rounds);
      if Train_Set.Is_Empty then
         Title_ID := Maths.Random_Integer (Current_Item, Current_Item + NTM1);
      else
         CLF := Python.Call (Classifier, "init_multinomial_nb", Alpha);
         Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
         Python_CLF.Call (Classifier, "fit", Clf, Train_Set, Train_Labels);
         --  predict_proba() returns a Train_Set_Length x two-dimensional array
         --  For binary data, the first column is the probability that the
         --  outcome will be 0 and the second is the probability that the
         --  outcome will be 1, P(0) + P (1) = 1.
         --  The sum of each row of the two columns should equal one.
         declare
            Titles_Batch : constant Integer_Array_List :=
                             Slice (Labeled_Titles.Features, Current_Item,
                                    Current_Item + NTM1);
            --  Y_Hat predictions
            Y_Hat        : constant Real_Float_Matrix :=
                             Python_CLF.Call (Classifier, "predict_proba", Clf,
                                              Titles_Batch);
            Picks        : Integer_Array (Y_Hat'Range);
         begin
            for index in Y_Hat'Range loop
               Picks (index) := Maths.Random_Binomial (1, 1.0 - Y_Hat (index, 1));
            end loop;
            Title_ID := Pick_One (Picks);
         end;

      end if;

      return Title_ID;

   end Thomson_Chooser;

   --  -------------------------------------------------------------------------

   function Tokenize (aLine : String; Dictionary : Dictionary_List)
                      return Integer_Array is
      use Neural_Utilities;
      use ML_Types;
      use String_Package;
      --        Routine_Name : constant String := "Support_12QS.Tokenize ";
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
      Words := Split_String_On_Spaces (aLine);
      Word_Cursor := Words.First;
      while Has_Element (Word_Cursor) loop
         Word := Element (Word_Cursor);
         if Find_Item (Dictionary, Word, Item) then
            --  the word has a feature so add one to the corresponding feature
            Num_Known := Num_Known + 1;
            Index := Item.Value;
         else
            --  add one to the Unknown count
            Num_Unknown := Num_Unknown + 1;
            Index := Unknown_Val;
         end if;

         Vec (Index) := Vec (Index) + 1;
         Next  (Word_Cursor);
      end loop;

      return Vec;

   end Tokenize;

   --  -------------------------------------------------------------------------

end Support_12QS;
