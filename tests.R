library(googlesheets4)


gs4_auth(path = "regal-bonito-471216-f5-a861b85bdf3a.json",
         scopes = "https://www.googleapis.com/auth/spreadsheets")

text="bob"
qrcode::qr_code(text,) |> plot()
generate_qr_image(text = "bob")
