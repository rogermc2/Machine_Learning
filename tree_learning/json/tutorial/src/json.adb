
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.JSON; use GNATCOLL.JSON;

procedure JSON is
    JSON_String : constant String :=
                    "{""name"":""Pingu"", ""born"":1986}";
    Penguin     : JSON_Value := Create_Object;
    Parents     : JSON_Array;
begin
   --  Create a JSON object
   Penguin.Set_Field (Field_Name => "name", Field  => "Linux");
   Penguin.Set_Field (Field_Name => "born", Field  => Integer (1992));
   --  Create a JSON array
   Append (Parents, Create ("Linus Torvalds"));
   Append (Parents, Create ("Alan Cox"));
   Append (Parents, Create ("Greg Kroah-Hartman"));
   Penguin.Set_Field ("parents", Parents);

   --  Show result
   Put_Line ("JSON object Penguin: " & Penguin.Write);

   --  Set Penguin to a specified JSON, JSON_String
   Penguin := Read (JSON_String, "json.errors");
   Penguin.Set_Field ("born", Integer (1987));

   --  Create a new Parents array
   Parents := Empty_Array;
   Append (Parents, Create ("Otmar Gutman"));
   Append (Parents, Create ("Silvio Mazzola"));
   Penguin.Set_Field ("parents", Parents);
   Put_Line ("JSON object Penguin: " & Penguin.Write);

end JSON;
