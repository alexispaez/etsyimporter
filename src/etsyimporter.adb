      --  Performs perfect CR translation
      --  while not Ada.Wide_Text_IO.End_Of_File (Fin) loop
      --     Ada.Wide_Text_IO.Put_Line (Fout, Ada.Wide_Text_IO.Get_Line (Fin));
      --  end loop;

      --  Puts all the text into one single line with no CR
      --  while not Ada.Wide_Text_IO.End_Of_File (Fin) loop
      --     Ada.Wide_Text_IO.Get (Fin, C);
      --     Ada.Wide_Text_IO.Put (Fout, C);
      --     Character_Count := Character_Count + 1;
      --  end loop;

      --  Output a CRLF in UTF-8
      --  Ada.Wide_Text_IO.Put
      --    (Fout,
      --     Wide_Character'Val (0_013) & Wide_Character'Val (0_010));

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

with Etsy;
with Etsy.Import;
with Etsy.Export;

procedure Etsyimporter is

   Product_List : Etsy.Products;

   --  Character_Count : Integer := 0;
   --  Line_Count           : Integer := 0;
   --  Field_Counter   : Integer := 0;
   --  Product_Counter : Integer := 0;
begin
   if Argument_Count >= 1 then

      Ada.Text_IO.Put_Line ("Import from file: " & Argument (1));

      Etsy.Import.Import (Product_List, Argument (1));

      Ada.Text_IO.Put_Line ("Import completed.");

      --  Ada.Text_IO.Put_Line ("Number of products found: "
      --            & Ada.Containers.Count_Type'Image
      --                          (Product_Vectors.Length (Product_List)));

      Etsy.Export.Export (Product_List);

      Ada.Text_IO.Put_Line ("Export completed.");

      --  for E of Product_List loop
      --     exit when Line_Count = 3;
      --     Etsy.Output (E);
      --     Line_Count := Line_Count + 1;
      --  end loop;

      --  Put_Line ("Import finished, lines read: "
      --  --  & Integer'Image (Line_Count));

      --  Put_Line
      --    ("Import finished, characters read: " &
      --     Integer'Image (Character_Count));
   else
      Ada.Text_IO.Put_Line ("No file name supplied.");
   end if;
end Etsyimporter;
