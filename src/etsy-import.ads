package Etsy.Import is

   type Product_CSV_Fields is
     (Title,
      Description,
      Price,
      Currency_Code,
      Quantity,
      Tags,
      Materials,
      Image_1,
      Image_2,
      Image_3,
      Image_4,
      Image_5,
      Image_6,
      Image_7,
      Image_8,
      Image_9,
      Image_10,
      Variation_1_Type,
      Variation_1_Name,
      Variation_1_Values,
      Variation_2_Type,
      Variation_2_Name,
      Variation_2_Values,
      Sku
     );

   procedure Set_Field_From_String
     (P             : in out Product;
      Field_Type    : Product_CSV_Fields;
      String_Buffer : Unbounded_Wide_String);

   function Next (F : Product_CSV_Fields) return Product_CSV_Fields;

   procedure Set_Field (P            : in out Product;
                        Field        : Product_CSV_Fields;
                        Field_Buffer : in out Unbounded_Wide_String);

   procedure Import (Product_List : in out Products;
                     File_Name    : String);

end Etsy.Import;
