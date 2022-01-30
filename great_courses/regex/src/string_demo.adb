
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure String_Demo is
-- demonstrate some of the Ada strings subprograms
  Test_String : String := "The rich get richer";
  UB_String   : Unbounded_String;
begin
  Put_Line ("This program shows some Ada string capabilities");
  New_Line;
  Put ("Our test string is ");
  Put_Line (Test_String);
  New_Line;
  Put_Line ("To_Unbounded_String converts a string to an unbounded string");
  UB_String := To_Unbounded_String (Test_String);
  Put_Line (UB_String);
  New_Line;
  Put_Line ("The length of the string is " & length (UB_String)'img);
  Put_Line ("If we append, ' but not happier', the string is");
  Append (UB_String, " but not happier");
  Put_Line (UB_String);
  New_Line;
  Put_Line ("The ampersand will work as well: " & UB_String);
  New_Line;
  Put_Line ("The fifth character is " & Element (UB_String, 5));
  New_Line;
  Put_Line ("Replacing the 20th character, we get");
  Replace_Element (UB_String, 20, ',');
  Put_Line (UB_String);
  New_Line;
  Put_Line ("The 5th to 8th charcaters is " & Slice (UB_String, 5, 8));
  New_Line;
  Put_Line ("The first occurence of 'ch' is at " &
    Index (UB_String, "ch")'img);
  New_Line;
  Put_Line ("The first non-blank character is at " &
    Index_Non_Blank (UB_String)'img);
  New_Line;
  Put_Line ("Replacing the first 'rich' with 'RICH' we get");
  Replace_Slice (UB_String, 5, 8, "RICH");
  Put_Line (UB_String);
  New_Line;
  Put_Line ("Inserting 'really ' at the 5th character, we get");
  Insert (UB_String, 5, "really ");
  Put_Line (UB_String);
  New_Line;
  Put_Line ("Overwriting characters 5 to 8, we get");
  Overwrite (UB_String, 5, "most");
  Put_Line (UB_String);
  New_Line;
  Put_Line ("Deleting characters 5 through 11 gives");
  Delete (UB_String, 5, 11);
  Put_Line (UB_String);
  New_Line;
  Put_Line ("The first 8 characters at the head of the string are");
  Put_Line (Head (UB_String, 8));
  New_Line;
  Put_Line ("The last 8 characters at the tail of the string are");
  Put_Line (Tail (UB_String, 8));
  New_Line;
  --  Count is ambiguoUB_String becaUB_Stringe of the UB_Stringe claUB_Stringes
  Put_Line ("'er' occurs " &
    Ada.Strings.Unbounded.Count (UB_String, "er")'img & " times");
  New_Line;

end String_Demo;
