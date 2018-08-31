{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text.Lazy as TL
import Clay as Css
import Reflex.Dom
import Data.Semigroup ((<>))

import Reflex.DOM.AusGov.UiKit (defaultColourConfig, css)

frontend :: (Widget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "hUIKit - Design System Demo"
      el "style" . text . TL.toStrict . Css.render $ css defaultColourConfig
    body = elClass "div" "au-grid" $ do
      elClass "nav" "au-skip-link" $ do
        let attr1 = ("class" =: "au-skip-link__link") <> ("href" =: "#content")
        elAttr "a" attr1 $ do
          text "Skip to main content"
        let attr2 = ("class" =: "au-skip-link__link") <> ("href" =: "#nav")
        elAttr "a" attr2 $ do
          text "Skip to main navigation"
      let attr3 = ("class" =: "au-header au-header--dark") <> ("role" =: "banner")
      elAttr "header" attr3 $ do
        divClass "container-fluid" $ do
          divClass "row" $ do
            divClass "col-md-9" $ do
              let attr4 = ("class" =: "au-header__brand") <> ("href" =: "#")
              elAttr "a" attr4 $ do
                let attr5 = ("alt" =: "Insert alternate text here") <>
                            ("class" =: "au-header__brand-image") <>
                            ("src" =: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAccAAABuCAYAAABSmNggAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyNpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQwIDc5LjE2MDQ1MSwgMjAxNy8wNS8wNi0wMTowODoyMSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIiB4bXBNTTpJbnN0YW5jZUlEPSJ4bXAuaWlkOkE4REYwMTAzMDIyNjExRTg5Mjc5Q0Y4MUU3NjAzRjgyIiB4bXBNTTpEb2N1bWVudElEPSJ4bXAuZGlkOkE4REYwMTA0MDIyNjExRTg5Mjc5Q0Y4MUU3NjAzRjgyIj4gPHhtcE1NOkRlcml2ZWRGcm9tIHN0UmVmOmluc3RhbmNlSUQ9InhtcC5paWQ6QThERjAxMDEwMjI2MTFFODkyNzlDRjgxRTc2MDNGODIiIHN0UmVmOmRvY3VtZW50SUQ9InhtcC5kaWQ6QThERjAxMDIwMjI2MTFFODkyNzlDRjgxRTc2MDNGODIiLz4gPC9yZGY6RGVzY3JpcHRpb24+IDwvcmRmOlJERj4gPC94OnhtcG1ldGE+IDw/eHBhY2tldCBlbmQ9InIiPz5xdAVqAAA9cElEQVR42uxdCbxV0/c/tzm9BpUipVAplDSJRiFCZIio/EJkSOpnKkJJZg2mZMhUiCIqGkgpDYZmSYVUUqiUJk3vv77/+90/u22fe+97776h99b381mfc+85++yzzzn77LXX2msIAkVCSE1N7atPQaFQKHLtGH8AKRJ7aMenRlFVn4ZCoVDkfuaYTx/Jvx5QilApZ3dXbq93yh4qVEyfmkKhUOQuRPQR/Is5FpLNAqENQuOFPhGaLlRSaKPQGUKthM4TKi1UNxKJ7NUnp1AoFAe35KiI/5AuTU0MbfRpKRQKRe5gjrrmGP8hRYRmxWGM0/RJKRQKRe5kjrrm6EEkEsG04e04xd7SJ6VQKBSKvDJ7KCj0iND+OJLjPqEHhQroU1MoFIrcJTkqDnw41YW+Tk0b5ggdq09PoVAocg9zjOSSmzpeNj9GIpFdzv4KspkqdGIiFqVSvrxsagudIFRLqLmQj/EtF/pc6FvSQqn/N+1eCoVCcfAyx9x4U5DeNgo9JFSE+2BUM5HSXY101tsvRFq8W7uSQqFQ5F7JMbcY5MwPoj6HvYW+kBurLNtuQmfzeHol5JLcbhW6S2i7s1+hUCgUihzL8c91JLsNQjv5e6+RJtNR76us62T+byD0u9Dz+tQVCoVCJcecjolCs63/5YQMQ8RaY5V01vuHUONIJDL//8XPSOQr2TQR+jNJL6OL0M9Ca4VuhSpYu6hCoVAoksn1jxH6LWSN8FehJumoM39a9qex7saUam08rm9SoVAosl9yzG03d0sMl4vvc4pkxvB0W0LaeYJ2U4VCoche5pjbIuTEskqtLvQUnPxDHsxhQqvJoMYK9YAkR2d/4/R/F/fPtpjZDKGeQlcm+AK6y2akUImQIqdpN1UoFApFMjn/ggSc9r9CqinPuSdYZWYKlaE7yHvW/uuF8gkdZe1byP+3paGdqKMU8kMKjXLa117fpEKhUGSv5BjJZTcHQ5lE3Cz+G4lEBnnOx7pkehIaI/jAeKlzWzrajHXG261dp0o9c7SrKhQKRdYyx9x6YwXTEPLt9BzU7qVWuxDPtYR2U4VCocheyTE3Bc0OkxghTSLDBpIX7xSaKZLZFznkZZwrm5rWrqXStq3aTRUKhUKRLEZTNURK7J9D23uI0HKnrYP1TSoUCkX2S465yVr1rJD9OTVjBph2NWffOO2iCoVCoUgWx4dV6bchkuOaHNjeRpaLiMGfYW4mCoVCochayTG33NT5cQxwjspBbS0stMTTxre1eyoUCoUyx2Te1Jw4zLGdR9I8R+hloQ+EBiCcWxa19YGQNp6Xjc+vu34aCoVCmWMmMEeprJvQxdlwQ5cm4LrxCBz/heoxxNyykHJvMOFxZrX1aKFdnusuzM7QdnLtFdnJnBUKhSK3M8e/EPklC2+miNCPCTDHhUw1lZ1A2Lk3Q47dks2d4iehlYjco5+IQqFQ5phca1VEh0kROiUL7+deoaMTKFdbqKzQzULdIxbkfwWhM4VgDNMt4oHsP0To2pBjZYQ6hBzDWiek6X4sd7mnbfuFxmZzv1gRRK16G3CSg/B5ZfVzUSgUeRXJDAKwg9sNWcTlkYD4znScug7rjbI93Nl/lVANOdbZc05hoeNCjhUDE5RjhTzHNgutFzpJCNf0TUZmCB/NbovaCUHUFQbB2RFM4TA8Yv08FApFXkWGJEfmUIRaExLRfO7ekgWMsbhs3kkHc98vjGiMbHtRGgSQyeMVoZeFLrTKPmD9/ltokVNXL263C33nuRakxovkeiYJc1hYuNk5oB+8wfvYJe0dIHSc0Eb9PBQKhSJ9TAppnsbT0AWxTV8ROk2oQmatX9HS9J10rPltMpKfbKeZtVHZtqCP4URk9bCus8CZBFzgtGMat1VsYxo+h4pCdYRe5b6+QotD2tU1m99hWaHjmB0EmUneF2op9LrQPO3lCoUij/Cz5K05inTxu2wGCC0TelcovxD2rRa6ixesmOR7eFCoXRrP6SH0eozjfwhNCnlgPShRQnX6rKdIFyFYe/YWekx+Q0XaIA1tyx/nhRUVqsQJR+E0vOjeMeqzgw08Sen1+iCaXeRUSuSLhWboJ6NQKBRp57RHU/p5Wqg1A2ljfyehw/l7MgbkJF3v5nRIjL9TmhscQ3I0+JfkKNvbhYbRJ/IRj+R4pnO9fXQbcSXHRSHtG+vcYzGhC3nNlU7Zv9n2m5BvMsZzyi+0Tai651h/26+Rkj7yVA4Selh7tUKhUMkx49aqP1NCfCqIGp1s5fojpMr1cLWQn01IGW043B2eScepn0tb9mTg0h8InS10htBcz/FZQnb98+R6mz3ldoXUD0b4GKP8jJT/vwVR61VIcm5cWBj9NBeCBLuO6mWfdfAxQdRQyJc4Gem6bjGTA2nrLKEX5GdLXI+JmDWMnUKhyNPIqLVqC6FWYEBCfYTmcHCHIz2MPKCOhNR4ZAaYItbzHhW6I51VNJU6fMHHa8EvMzgwuTFUjnXsQsI44OaAdcWK8nut0zZYy1YJommxYHADtWcB2T9atl+loY13pOP+wCihXm4n1/tYtrdL+5by2BHc+tS7ZXjPFwVRQySDekKQKKfzne7Rz0OhUORVZFRy/CyImvxjzWpUEPXj60hJC7iG24xYPg7OAGME4JaAtbNDXYlSaKbQE0E03+O0IOreMZ/7bSZYWjbfQbpz6oD7yo1CXwq9GEQtaDcJXSL0bRa+x9ZCC6R999AQyjzvYp6yU7g9ybo/uG9MDaLrj/cLk92pn4ZCoVDmmE7IIArGCNXdD2SEV8o+uEvs5ZoYnOu3u8wmDVLjnZRmMgpIUs0sRgAgPVRXSmBYS4QaEyriyUKdnfsEw8N66hQ535a2zRrlx5S01lJ63sjfuJ5x4chshgNVKIyV3hf6KYiqgP/0lMNk4G+jNaBk/iYkbKHxcq9TZV9b/TQUCkVeRprVqpSimlBqmg1/ONkHB/IPhQbCiET2gVnUJ/PtFbIGF+86VwZRdWp6AcZtxyutEkRVh0b6O5vMG0zroyAaqQbrgpXISL/n2husVUfKPcyQ/5BgYX17K+uozW0joU+tesvwegBUl4uz8J3C5QRrlj1t6ZDPFFI9gicM5KQl4ETgXEqUHaUMovnUCrI/ao9CoVAcPJDBs7LQS7TK3MnfiHFaSOgKWD6yHAxN5tIvMZ+JIMP9VeNcoxEtMzOCF5FpA+tx1r5fmDLKWHx2ZlByg9XcB4KasgStTBuyXW3ItI3F672w8HQCma8Sup8Bzl+l5SisVadncSzXlz3PdbYQJjMpJo0X29iavzvx3P9qT1coFHmMtyUn8DgZ2HIOprPgvuAcrw03CP4ewbRSYKDbhR6KUW8ZobVJYA7NrTrPENrD/R3S6MoRodWtcfCvapgjt7inHVYdO8gQXVeOidkQ7Pxuz7Otbv3P57yvXWSepfRTUSgUeZk55ktnJVirg8N73SC6brcq+Mf4xmBJ8I/adqjQc5FIZHcQtZR8PEb1Q4IMWLcSUOPOMn/kulB5mmgvp6exLqzFgbG1CaKWty2d4w253wC/q3vqgdHS1ix+3wOoSjWASnwRpWZIh2spHeM9wY0E/eEaeV5/6qeiUCjyMtJrkPN1EDX5PyGIGp5gfSof/e6epCENAnnDBeIMGWy/CKKuE8uDqNUopJPrPEwXa4IdknBfb3t8G9dzu99THu26LqQu3AekUKwzbqNPoI0WQnAJwX5Yq2ICcLKnHsScvSsb3jGCCRzP35jEYC3yI04W4C+5gxMcPK+Wcn8fUNI1EY7KGxWsQqFQ5BWkxyAH4c4uJ7PoYzGc5zn4wogG6Y4Qkg3GLrdCBRtE3T7AoGBJCbXqRE/1tyfpvnyh4ox7gy97BiTVq0PqGsT7GAtLXM9xGB8NlGM7+Hxw7/Dz9DnSDwuixi9tsvAdH0Jp/CxOGIZKG/HuEPavKi2Lh/H9nUTr1Xqc9ABYu8Q7b62fi0KhUOYYjvZkJp9SCvwliLpCwNEe/oLfcNtPBt6+skVA6xMpNWFwrif7+3iYLpjWhUm4p6VS/xzP/q2U6hD7tYpQL6yx8Tf8AU8T2gCpieUPt36DsSKYQFOrvirW8TaeBVw3Jdb/u77A6CWIuracmIXvGSHumsn1P7cmMzP4LAIyyMvk51t4j/If99Od8Vk7B1HXD4VCoVDmGAYZOLE2NdJhbAix9goljlVkcvaaH9YfYQmJ9brnkQVDtl1lv+0oDxVtMmKwvhrj2AJKb4gcg2v/xX2u20I+SosLYtS1Lc5xJGE+yfP8tvA5jCVDzirAv7Ehw81dGkRdPo6l+hQGRyOETpb2rWJ5hOuDz6rGW1UoFMoc0wnkM9wbRJ3QC8iA2opMc5psfpL/VzMHIxggVKtQO85iFouBjMiSrHWtUTGODZVrraSU92sQVRXezmAGhtFDvYosI1tkf6ivH7J1xDlex8ccySB/pzUtGBBUyRWy4F03kGsOD6Lrp83JpKFCRbg/rEPCRQXuJi8F0UACWDfVGKsKhUKZY3ohA20vMoRljvSHaDOFGDQckhTSWo1AUABGqsGgjHRPrwXRDPQZxWqpe3WsAlbmCahSOwstRC5D7sN6IYKb1w05F6pfrBdOsvZBfYr7h9EL1JALhRobhsdMHrs8zwyTiUH0R4TUfXEWvO+2lA7Xsg2Q4JFqC1Lj9bx/3E+qHNuun4dCoVDmmBwmudL5/5AMvDDQ2S6//yu/sdYIt446kFxkC6tIhC6DtNI0CU3Y7GFokGK/sXZBKkLOQqhW4dpwCpkamDNyM+4OY44CWOH2D6JGOAGzaCD6jYmYA0nygSAaX/V77hsjdF4Is8W64y/yLC5BcAT5fVMQjcCDoOXwNbwwye97vlzrZ897+9a6hxwPeVaIQvRleiIv5QUgyENwYEB9g03u5JFB+W8IoksJw+T48oP0nmEHATctfFPQTEErgmWTVUE0lOIXjoaoo/wfob1FkV0d9iKhH63/ZelsfhizzZ/O6DqwVBmfBKf3rz1tgF9fOyef4wfMQQkgTNwo/l4stF+oix0QwKrrcSQ8ZsLgaQyC8Jp1/fbwH2QOy0ZOEIAenvoeoeWod4AT+sO5P7jJXO7Znyi+zwV9Cn1lKd2FFP5nVJn9cr/z/l91ypUUWu/kPi19kN1rc6FPrXvAPX8p9DZdy5Za0a/w/TTgObu0pyicvpTUfI7xAGOPvdZ/SGVQpWLdEdaukLxO5IzujyRcb6Vzs1ARIk6oG1QglW0B4LB/GX+fyBlnZSN1Ct1o5TfEDPQoK2sFjFqaW/VCqoSrBmbjRRJoL85tFyKFb6NEbWOf7IcKuLFPSk7k/eeCPnyOUE2hbk4Q+Jz2oXW2rJmzFNAOCP0niLpPxcIplLIMoFY/7SAZyIpyfXxa8E9gDiQTgLV1Q6H2QpcJHc/+AgNBBL74kucUVnaQpxnhcUKTYpVJb4ScRCPYwCDlGIZYg78dgltDeoNrxP1C1aTzfp3Ee17t+fgDh0EbJv1J4A8IsC34xycSzOS5IOriAStXSF7NaFyEQQhrrHZkIKiykBQZalz4PcZbtysTxHbp2Gb9XhdEoxLhumiHT3L6moweamOok6EmhlvNYg4ct+WCfm3iviJA/MU59MMDk3k8BzTlxzjH1yS4L6c935KcXF9r7YZUfKF8Gz95JgvLwCz5rZpvfouyiDzLGCEADYs3QUqv5PiNCcAdB3DxyE9JCgwJwQOwplZeOusDmWD0scz5fzS3YA5YfzC+h1h3K8RZcj+HeSJF1lP8fS638OeEwQrWGncGB6bRQh1TnQ8V/oOwVjUf6mFklv/7uDlZmOFh3DZaWL9hHfsHz29IJmwD60UNhN6Fn6fQFKGXha4Qqi10gdCEg7xTQwtwpv1McmAb83NCUjYHNGd7HAkTVub3sG+C7pN9C3N4H8DzfYcTQAN8ZzfZa4oh9wvDt1sDRV4HBLPmmdVBZzBI9QlxytWkvv+YBOp8NQlrjkc5dV4vtIJZQZCOaTT3w6F/HH9jfeZznr+WRjIBs3JMCrnOKK45nsj1P6xjDGEGkG8YdBwZMGqwri/tAAIs8xrbMSnkeZRn5pNUBm0vxbXQOdw3g/UYXJoJ7xnrweVz0MA43PMuGuakGanQ81bb+mZzewbHWnO0GQ6ZzsEwQbrD0wduTGMdn9DXWpH3pMYOVr+Z5hxLyprjSIqkTyUwM31Ktj8mUOfhGbzvGR43DsQQHc2wb1DpYg2xHI9B1Qs142pKhPNZ/gfr/LdCrrWBW0hvN1BFAwvVS4KoKhfBvmH1dxiZLSRnMMjS/A/JcxByRAZRVwokUb7IucallmQPNS6kxsFBNPDA8XIumO1kq/z3Gegw+WwmCLUxBtYgqso9Pod0avQPX9zdHll0/YqczJxospkgQwtSoFkSDVTwXTN4HbyLWpSSfccLcNLZROi0ZBjQSF/CWva+RN4BJiPMZFOd6qm03l9BTh6bWonAEz23FCVdF++ksRl94mhsTDuP572emJnr28yW04jPtmg666ggdKrJIJRA+WK83immD6fjPZ7Ma5YNKVOVxk9VE6zzCPbpupadR3q+0eohxxEn+/VkvzwwkruZwaEQ1YKzyH3PYRmEU7vYWKOmoVOcwByP6zMoNd5p1fuZ0FDP9ZDjcbTQA+wcT1ECPMmdOVNyLERrVhszaHU7zVN/E0qJyIZxiFB/oZlC9/H460II1VbMOqc0Z7IXW9LHGU4aLGNJ+CgtXw3Nto7XScd7LUeJfRNyUHJfCd57aqzUYtnAHB+E5TOfn409YWvgtE5c4NAM6/hbnuOjPM9oMq+Fa++m5SOsnTez35exythYb9crVInl3Gs+wGthUFhonf+A1Q4MGmOEtjq5TmGd+R4nDwlLjrSUnudpSwWnjkP57FdSk7HPqu8nobZO+X6eOk36t2ud7xx9vEsa+sAtnmf8XTr706CQ/bASH8hvAviNz3gjc7cWY7nzQp4f6H3D5KA9ct75XKNFkm01oQ/5TJETdpsQgnP0ticeCJYS1o85kX3DskxeQsb1rq9vM5/tg7yW3U9bJPLtcHy6keORAdr/jBlD6YWw2HlPU9y0hta16jHf7X6e9zef+7VOmS88bepAJux+f5O4Nm00I094+s42p66053MkR1/LGd94DlIl+VLmc4DHADHVuvAmukycZNVTmAz0HQ4Ckzk4v58A89vEj6uZ59huDpKo6xh+oF2oupzoSEQz+KKqcN9JZJBPmYdpmKM1w2rGxMm1Tae1mSMH0OFUq1bjPsz+v+aAVpD7jiQDvYLqt/yWtACp9hUOvHanAwMcRGbYn64kGNTHWh+wwQo+SzDnc+z78bzT6nyPQH9r/7vcN4Gdqi4Z/En8mGeaCVEWMsaiVF93Yd9xESs/6ECn7J+OGnSMc3yBI6UZ1fU8qraLcfAyTAJ94hq+D9fFZhn3jzWDMSdcS12mxf6yxdm/1FIl7iczqsV9DZzrrfTkVB0cx5Wjs+dZVnFm/qY/9mTbi3veQQen3gc89T7F79SXq7Vpgv3gE8+5ryaxn5Wi65dBG+5vbk3KlpgJBF3VXCy186Ra496PXIqqw32Nrff9pLUMZSY+L8Z5pn+SCfqSqJ/Avv2R68rFMXqbw9xS2ZbDPQzSLTOUk5qvPK5C95Bx7uNzcif4b3meeVv2i1QGizHM1eA+RzLc49T5Jtv1i1WPwbs8ryG/wWXO8T+s73NsmpgjHIS5xlXfeskQ/8/j/5P5kea3BpvzKaG9R25dizNm+Ah+x1lQHec63eIwxp2GyXKm7gIM+1wyud1kPBUp8jfxqK0wA13HDz4/292Zs5ar+H9BnGczjefezFnHxdY63YN8WVeEnHs3nyuY5Z38cFbxGZ1jrYHeww/mPt+aEAflVM5qV/AFY0b7EJ/DbjLoQeyEZXjeWfw41thrlQiKzvo+NRa5/NDe5fOsxnd4ehYzx66cSRahBJXq6eRFQ85tF8YcQ9awFoScO9Q5rzv3N3L6RGq8NUcPcxnFwWSxM6BM42TK4GfzXljPy049vdPIHGvGYY7zrf2XW/vrO+esceo9x1Pvt7AJ4Lf1nnPslQT7wSZPvQ8msZ+9bdU71zk2ztEemUmy65+9zmWOLLfGYoIVLOa003mnY6262lr7L/Awx4f4XaxyNAlH8px7Pc9rHLVDh3i0Yjc7bb7Ec/4yY9sBH23POP2HSTTPSYWr5Slq1V+L56RyvCzg2GUYAaGBtX+lU+d+o32gxi3VES5KWef2dY7HXHOM11lqspG1KTnsIFeHeHs1P+opMYwTWlDKmUumU8g6fimZF2anP1BdFIYh1nl1Pcevto63sgaZvrQK9bXvKHYUzIBqc9/llMymJ8Ac55PBDTAMhhLmMkqAZWKc24Azns2UOJtZH9vJltNyUTLt9VRv9eBHVsro0Fl2MtVBD1HaMTOvKyzJ5iUy0HWcwZ5tf8R81+v4zApSourHY2U5CSgaZDHYj5bZjMYxRDK4LuT8tnGYY48YzHGwo53o6LQLfeeMdDBH1/hsL1XuEX5nn1Gz0sgze38ghlT8ShqZY5Uw5sjv0lUR54txnr1U0MJzvIZ13B1UZyeoPfChW5L6mTvpet45fr9zvG3IealMLGCfewIZQyX+H2aVne5ZPvgfE47xTPdxslSO3/4wBiXpHaNvb2D0JHN8hHP86Tjfzm57/dAaq2yc66xLurA1iR9b+99wrm0HVxnpLHfZeM46lt9zvQbpZY4FEjCoqWdmO7IZH0RTVMHScwX/G906VIc1acBRnwTXimeY7NjFo1wUhzM7HOFH0/DEhw+t3+WcY/vZHtNmMAqYdsPpF47QXbge+ZZt6k3jnTacEX/EF/Cr7AcTQNBtqIFgcHQvQ939T4UaRFN0IZBBdzm2gmq2YcZAR/Z9EvIBwvfw7iDq5oGXWs24Z1i4n1uUw/v5XMoczvNhLAS3EeP/aCYbGxA0gMwMA82zNA76mAELFgkNZx2wHMaHDZN2MM9npQzC1cG3szqDDwTOwj6CHWDW+0UQOxNJZgAfG1xybMkN78VVp2HAfSmeOX8GgP6NZQS4ENwm18EE444gmgoto4BWoDfbDleK060+c4pT1v5m3WsfmsT7hTsSosgUcfpaGFBue4yxxHazcn0ME5l0hRmN7E7S/V7v/P/V+f+b8x8TcuR4nUUp035PN/M7NUCauvel7BoKCJ2sY6uceu3xAIy3uJz3l6e9mKgMkWOmXYkYgv1qvm/CfV/F45y/wwkR6gtEstR653sYiaiI20ehiQqiAT0MXP9Uu24IPJGQb3u5bVgGSdTpT8XS2yHSYq26n8wQzrSQihClZSOYDC3m0CGgo8fMHIzmhSDqe/SFY9gDnfztHLyPCKJO3YjtGRZYAIP2TOt/Def4XA+DgR8jfM0wy7k3iDpkTzXrgc5HO4pMDese0JcjYwUsV5uREcygG0gR6sQhKY+XMq3JGC/mvaMT1PYxRq4vzOKAjmdYUcrd7babs5wLOUAi5iwG3/mcGWJSUgqRTxi0POB7ANZZgyzabiY1L1PSXm3Uy7QchnUu4sBirQ7rJog3W99hjGBA1agZwGSho6cDm3W5cpnIHNE/RkrbNlj73ubgbQNuRWcm+dozPPsQ/xaaEExspgnNS8J14KT+S8gxTKIe5DfwEieVAdW5mZaAmhPC84NoUnL0gUtg9U2thc91ongGLpeI1eq2wB+045AY0uCplH7aUuXWgtSM9hOGMJi6fm8uA9/p/G/qTNZstLUkcEyqOnPCCjRwBu9KVhsxaa3ljM+xrMU/zeBr3pkFk1s3F2yK5/kB1Z3nYLv/mQTyiWB3gpOqxJgjddDXcW3qWkf3C5XaGA7AyJ4B6QQf5nx+LAVlCzcGzKh780NqSgZZw8Ngf2ZHR9BfOKjPIlMNm5nCud1+wCc7x79wjEwW84OGaTrcEd5iOzC7+4prfK7EXIadEm4BL3ExvKRc16R4akJJEQz8NNn/HtcN3guipuWQ6L6W/Ts8hkwwM/+EbaoqZZ5wZm82HuG2F91PBkAaZ/muZGo2KnL7HQc0zOyuF5pLZo02Y6Y3jM/dABMTZCdZy/ZDWi1l+QxCUhyI90OGA9UVFreh6utqrxVzhreBawbdk6xShYQNFdUgZ+D+m/fkItluHQhi4UucDTU8DHTOT9J1vovBpPYIQXvRVOg6DiLoS3DjydRwgHK9TzkJbMVJ2gD2mWT7luZLoC17fZOzONJyfn6zT/Eb/Iw0nZPIEZzsg7FXds7dE6dJh1quKKMdSRP3cwt/X0TNznTnm7X7Ug+LjmX7DEXS029yMMzYW8nZ38R5DiWc55DeJZ10LwUVoCP/RHJcMDbEQ4UhQmuKsRDpX6HEg9+zZP86roEN4wy+DyW0MZRY4HOHzBtLnetBxH2d5TEgP0wXglIh7cPs7QlnXyPn/2JndrmUbWpIKexBSh29sK7DQRVri52s9p3KGc1Gdla0aTFzNuKebgDDlN/fcF0IDKIvGcjlVIWAMbxhDezXUhWJmW0XztJ6czEbHX6M1PeBVb49GQGe70SLCazj7/Ge52M+NF9KrP6s9xD5PcA59iAlwpvINMHAITF/ySJ7yUD/4nMsyXKbzCSGFqtPc0I0leURCGGX1PNCEqVG3Ft/zwK5b023NWImMrxeMpgD1gIvYN9v6JkBY5LUghO8jCCuapaWhIM4mC/jJLGTo85LOrgWfh21DIdSapwcwqgyG1PJPGwcFeP9QdqeyYD7UzzqS8Rg3UHJ0WVCiUTvysfr7KbB1gPWsWs5tnW1pEbfYP2dnN8iHc8CE+HdwcELV+KfLvfTIadx8WepxruYLxmMahEZy1gO0OOdDwaO7OM426pJ66jn2REwSztJzqtgle9A6aMBZ3OQbkZz4OkUo30P22mJuO7pOniusz6GdWRWAAb6Z5yPBSrdZmQKMCrCOg/abaxnN3KNrhf93V6hEQbafqX83k7mioHxdSn7OJ9XCTJVky4I6i+kVZrEGWVdDuYXWuqnTlyM70XJc7C55zS8PxMeb0eMMsO4bnUvVci2WqgDJWMMuLZRRGXOhqHCu5uMAQMyYrceznvAB41E1kba/ZKuBDdRpZ7RQbkC24UJxoYYKhs7jF6Ek5Sbkyg9/c4+8wT7gY2CfEZNM/MjleufyQkMni/WmFpKu35N2Bcr/dctyeu24q5umPjYFq1ZjFfJqIMYmiQffEkNVhhND755GPDFUtF6ju13JjXDOOk3GrCSVIefTE1MEDKRhbFOPmqK0oI9wcENd8w6KSeKuBBnz7FmITs5IyoU8sGcQNUE9MBDMUsnc9xiqexsi6hnOWDup44cgwzUsRhAL4vRtiWUzA6QDDzlCqZxsMOIAotUGKGAQaNOrLEhP6AdHWcjJbMRVMFg9glGeQfvEWbOd7OT4t520xptEtWZRzOZsP3sipLRnEQdOtp+Gj8i/J8XIiH63kNjq0PFmkH2JmODmTqk+d58DngPT9BHsLTDOIfwHntRrQrjquGUbI0a6/+zHMCs3woe/2mcd5oWgMF9b9ob8gxmkGHb+A/yhlqTqnSHCYPjMSdMq6Amg4+YbF92VHkN0zm4JdoGvOMPLYnjUTDGzB4YKDGOsSYfUM8Pzc7BisYvHzvjAKyxK5oE3mmA+83M41ho4K6huuPhEvudwzCGfnz/scrA6G2QE0N6uUct3DyIn0Elt2GFZ5JwrDMGZyvABMEs7NBRZ5EZvO/5YPAiJ1Dt9jB1w+gYSNh7EQxUOBvbTN/GRpTshlGaSKHEOYUzwLDFUjBoJCN1Z0dnecre6Iv/SWflCkxNUt9ajMcCfApViFCnHsOZ8Uinimsp7X1CBgaVLNJrYXsc1TsV2fEXcnY6m4N1a5cxmhkq1h6EIF3DuXUOr3OGxZTiDVqFmZfStuCtF2NAwaBxBZ9pL9fMnG2f75yDjx6+rNPJGJ9ge/H8se54npTB9a+mqtUAUvTmJAzMpcgch8YZLL8J/r0mWCw40HJvdxwJINaCfa3gnywguN771AL85HxDkUz8Ru9zVHFrs2hsaOFI5WszawKQRnQNDrQcjXDyneZu5vwf7fx3l3rcMeZDT51DPNd41tm32PlmgL6+UHxwU8D4lUuZoy/pQn/fJI1RzQ7N8hbSV2uIZ39lOqG/SL+ssfS/GmNFtZnGMHIL6IgKFdT2DIaB22H7yjht+iLkHOPzs5AROPbEucZeRoq4gAG+59EHsLR1LeOD08nTDuMcXZ3/H+f/l8PiTdKJ/jKGgJvF9trYHuI8fBh9Sp+mX+Uuz/3UTeA91+B1l/muE3JOPvpZ/pf/a9E36ghP2fx8Ls8loU+aaCBHJlC2U0hQgBQeP9JzvCyPVfVEtfneqvsiX4B9vsd/OYt7nNsHm0kltQym3JuJRnnxOD3PY8SPqzxRTqbynALcDvNFDLHqPtrzbEyEp1s931hnWoB+7DnvOOu6Pj/HgtZ1r3OOrUpj/2jIwBcGuyjlh5Wv42nPWM9k+lfr+GvO8ddjRZOxtRlWuQkhZR71tGeYFUkrH8fej61zLnTKJ7JOfWeYDy+Pv+gcH+Mc/5efo2dMcXG0U+Zvn38oj73lOf9ey5+2IMPSPW+d861Tvqdzvd0xruf26W8sBvzov4IAMKIKdO6nORd5JjXrMdd2EvW87KGZcM3PKOX+SOd/Y3BiQuFd4mnHR3wJRflgP2ZUmQIh7X4zgXZsdBkro/7sinPe5LSoyjiwl0/yBKsUo+j85mOcaayrjnXPlRMof1jIc3ncKrPU887vp2P7Sk+YQvNxtnKztXD/1b7QaZ5BH/XfRIbW2ir3qVPugxj391nI/f3qCSawgyEbr+S5HzrH5zh1NwkL5caAGGEY7BmEphiG42Q+SHXfpccZe2daA3szetdXDsNqHyKBnRqPObLceVZYwN+sYBspDuPsGKNdl1rlWoeUKemZHJvoOh8xKMoGEzQgpG+lxgvK4QkS8bsTs3Wcc3ymc/61YRNLHm8WKxRgyLd5vSOAbfaU+ZHP4RdG8SlhjV/uZPYJq77SsYKDpEYDye93hKReFPgedpmjicoCyWAAVZ9QGy6iA2dDqpGKBP8sQGNtsVyQARNZR6W1i2qG7z36eBe45umWygPn2m4eW6nK2O6oTQpS3VaKW1A1qgzx0cKI52eq6GBFeh0DAMAA4wjHxy7gTOYYmLjT9xHuELVgvOHpoLhWA67RlrDWc7E15Yt47sUgP9clUiz13Q7e3w6qS/fFUB39HRzohBuGkjHUg/Yxcw8lSGX5TqBWvoyqzvQwxRSuvXYJ/nHcxToXDEJmGAteu6NzrQZahjCXCqwXP852TnJUqujP/dhnBnnOg/X1kVT/7OA7e4/lr2QbB0u7etrScxA1xups1QXV0V1SbiA1E/DxvZPv1S6Dj/wN18Kba8uTrbbjnb5FdS/qgDRgB/pHPTBi6skyBZz+gHt9iksFaIcbsB7LAgO53oglBdvYCO8DgS8+5jdgq6/hVnAOlyr68/uygXvozm9hcPBva2PcU1+pe3ka+gz6ITQ7fYWqcPcqtnstxwtod1qyDxu1NPrUcLnWt546L+E7LMUlB0jbMKhpyLEPvttvxmgTnveP/O6OC1NFM9IMGLQv7R/W3drKuUuMdiSIWsJWccrBPuEFKTfOU//t7MOuD+nLfO94F32cfriP/eNhLqv4rjmJ55dhP/H1n35UfT8aHJiTNuA4C+PAN9jOBvyuKnqewzd8DmtpBIZ6r3LKbOY39RmX79ylt1W83gheD1mUnna+C7NWfMAyXsT5CPuxI+3jWmE8c7j9LJPfs+5T0MPYCgU5B+jo04KoL9sITgbacC2hM9cGJsv+sp6O15fM4GOWu1LKjbWOGwvQVsGBzqw5EX8F/3au3uJ59759v7OfwBDnHUSNyYDECAbeKOTwH2agsNXUQTTIQzys5Md1LAfzI/mBImLS4hj1LCFDasGP53SuweF8xBJ9NywAAD/4hpy4wFfwZ2sCUD9eWz31QYJozQH3Mzs1G2fysIwuzknEt5R4YmVpWcJBLyXk+DYYWXGgx8SjAs+ZaQZ7SiBnkgnieUyitXudINw1C4Zhh8c6LnWsT0ffyce+05T2AKX57szkEUwdBiCznAgvYfWBocCy/BROTDEAQ33+gZyfiDoTAzj8U9+KU64AJxQt+Vy2cDI2xvbtZmzrsHf1p5Rd4Km7icMAXAZWJ87xGjGuuZLHwtIMYmzc5mGsB/Qv59vH825MpruJE5zxJpUa+/mJYfWRCZ6Y4PWqsO+CH801E3rX+tungkDHcn2HCsR4UPFQMEh/CJ+CnFEcz9/7KGH+FlI+whni7uAfU2EzqBuGXYgvoCql4jqc1V5N/80hlF6g3kUklkZWRJr/qZw5+xjAj+AKS5p5lgMqZqUIUADjjQ0OY9kZIiXuDf4JDRckwJyMFLk7hOm5EuXWHGJUoVAoFDkKcZljDmvs4ZztN3bE5clUb/xglY1wtlOHDLI0mWB+SkZwzUCILqitFhjXFVrUQo1Sntf4ntIgZsNwb/jFVltQdQbxHa4NWHCvAbUrY25O4ewY/mDu+o5RTaaZQXHW1JQzNROI4UftzgqFQpE5zPFganhdx5BhCxf2TW7ENbSafZ7phNrTBNgsdKc61m2fcJHbpG+ZScOLklbqk860kLVTynSjFddkYylFw5zVNOgpwkj5N9BIZYXHqGYTM0u8QYONGiH3jPBzI61ccruYXaO2dmWFQqFILnNMc7LjHNDoQSYrNH0WH7RymH3NBJtuBvMOzLVWn4zoNjKtI2gFOpgWaVtoqVSXlkwm7NpoMrzrrES1R9GlpS6t2kyEeZMa5nJa+f5Nt4ZHaXJ/Ky2vnqdVKvIztoTlFk24l9J6bTjrMPWinf9l/snzmSKmvHZjhUKhyAPMkSa6T5JhDaafm53/7V5Ka0dYTG2omzTZKt+M5SI02W9sHYO5b3+awfcm81rEOncx3ZUJYr6XTHUSGfIs7key2s+tOhdbvoqDaPxh/OiO4e8ylAKPoTRa32nzYZSCp7AdUyl9luPxImS6XzjngeHfTikVSZK/ZAxZhUKhUOQU5kgJZzsZTjc3gz2lrNVUZbazfFjg3zbbUj3+hYS8lrT4ErPQd7Oi4fuun48Ji/PTP8lOhlmAUuQeKwt2XzLRKyhFArVY/gW241zuf4wMDxLmZSxjHGU/Nj5+vLePWN8fdN6vSIf0LmTQU+wEsc49lKIE+SZ91z4lo7yBcT5NuTZOkujNZOLFtasrFApFJjNH5mG8184CHad8Sw7qqZSybJeRpsZxk8zuAufcGjy/PdcR11BigzP0YQlcuzONYwI6eN5nHbuEzssnW0zXzkz9tJUxu47FCPdSRbuOjOovhnFrbzlDd7bqeYJMuDzXI6db0Uue5Rbn9kjgfiqSka6jRDmagaiNmvcURiRBBvnCMeopYDHoiH4KCoVCkQTJUQqfzsG5p+dYPTdaA3z9rOgHVzvH2lqSV1lPZJgWDMM0jb9hNHNXgu0cym1+qkqftY4hnNKvMc6dSakS64Z/kkmP5vpmhGt/qVRhXsroKvWo7uxv1XOhYcD8fzOeBX+PJ1ODhPthGiYnPcjYbqJaFsz3SjeyiPy/hm02obwaMALEDkvCvCXkOlUSmYAoFApFnmaONAA5xGY8vpBqXLvbRJVeYWs/1gjP4XrirdaAvYhraoUphXXkfoSEmkCm08qq5yZbMovRXjCwp/n7eBOL0jrexo4f6Zx7Do+N5P8OVFk+SeObclxvBJDK6icTR1C2bzM0U8mQdkHqnsbf88y9cO2zYoIvDha0j1j/qyH+Ixl3TyuW6HNUW+PZPuTExsSzPctTd34aJa13mSOf6c3ZEvhXoVAocgpzpCFLPwYSTqVBCtKJdCWDWEO1Y0nnvIs4AM8JCUxdl9abPSiFVuLaGuIZnsk1xd0cwIs7535jx8iLcXOHMXqNMYYBtlnHC9J4Zi4lvoLM3HEbJasfbOZAa1KD7laQ679tgxqqK9dThexK0DU4cTATgM3MqG4Mh1ol+OKMJNzf2d+GE4wtNAY6mvdVncGLu1P6Lh6jbhNUub3n2HBeV1WxCoUiTzPHN0j5aDBT3goUfDyDxX5q1Jc8B9HV+9Bqci/dFuqHNKAoGekYS6KBz+AAumW4zKUcyyWyPgdm28f6v9ATTLeyY/jzvwj6IUzdZFoYRQkx1afipST3NZnkcD6zNxhU+RnDXHh+H/6GYc6NaXh5Jpp/I2c/JhtLyAy/ZTvrk1HuZUbysDq7GWmY/6+gtFyAv1Oppr2A0vTRnKy0089JoVDkGebIQqcyNVFJWkj+x/YjpNRVx8pg0dNEoOe62IV2mh+uKzawBt65ZIjXmbREVD029rSlTRqYI9YmH3aYSe+Qsg0oCV5u3C1CykGy3EbpDNvldvodp2wBZr0YznXJR4xxkFXmVzu9jisJOmULOv+PYDue9JTtwQlKc6p9R9KC9uewTCd8L1hbHcL7rEBfzHWUhlczMEIFMtzX2CcQRPsU/ZwUCkVeY47ncVAsRynhM+vYsWQ6i+gQP4zMYB4NWqZRgprOwXkcpa6nyXB/5mCL9bZHWWcRe23QaUtnqjFNnrnWdJNYQBVuV8cadrjD2Eol4aHZ6bu6ZLCuls5/N8fe4WRsq8icfuLkw6RRutX1deT+fPRx3ED19LFU215OqXgE3UjsSU4lBuENKHn+ZB27j2pmaAsaGYZOg5/T9FNSKBR5jjk6JxSl9DiOebbWMOK7W24TmVWpsDxjXNvbSnXi3VY0mhZhSXI5GPewGMM7ZJCP0w8REuhjVvnxYYYxGXhoda3wbSlJrLccI+YU5v8UqkX/Y6lha3NN8F0ywEiMZNCd+FyrUxpMNRF1aKBzLp/bq3x2t3IiVInbdo5kXZS/oZ5ezd8wPHpbPyWFQpFnmSONQEZaUtNiOwM2Jb5pXI+6idFmkCDzqJD6GlkDbi1LLQt14Hkh5xjGUZoO9bUp1fZnm+pR1ViS5UZxMMf2LjCKJDy08iZJbhJfBBjhRKowr+e+jlzjTKEUOJeh8fJxnbRzAnWOo9QHy+GzhGoa6dB9rlSft+e7621bsrKublw7nWOpzQ8xUqxCoVDkOeZoGd6YIN+vuJFpOFCOJNM7hIY5o0Pqq8W6WlkuETfyGCSZMnEafjaZyY30gazIOspw3awI1aibqQZ9zTL4uSfBhxNhfdW4ptqUDAZMaxkj5BTJ4AvIT1UwjJbep5T4O91mbuE1jKRqMnGfQFeLVxOofwInBytotfpnDJV1ca6Bdub/htQOtOe5HWmpavrA6foJKRSKvMAcC8QoCwkPmaSxxrbaJJ20IfuQS7ADpTsk/oXxyXG+yphYtqFsvzIDcRDNYg5AgoQFa4ocHxzSHrQV7ghIOQW1LrJgI/XTeyTkR0S2ZyQvRtLXUbQqRTLRajRuSWX7kHfxGBJ8FSsH0RRXKcE/eRZxv39Z14e0BAkP0hT2I0cjUmAhtRWSfyJ9FtJIIWHtXzGeK6xCkdkb2diRlRuJPVfwfpC5/Y4gmkAYiYTnsx1IAAuJfXMC7xjJWBFibg8TOCNB7/aQdwKJG88MEx/kkvye94/koMiKPYL+mQ8G0eTNYLyt5dh0/ZQUCkVuRihzlAFwWRrqgWQGqaKHnaXcU+dX9m8r7upGMqqyZJwItl2Xx75jJnUwIBiCtAmiORMfCqIZym8Vul3oAjKFelJ+Ic9FzkZkpa4phEG+HvctJIHRIkrNKqFf5bxNbBN8HcuT+Zns0rvYTjBBBEaA6rgKt2CuWK+D0RD8GpGYGDkhl3C7iNcqwH2nyLW+tGYsD3BygYDlyPo+iIz50yCaM7I6mVPHBN4FJjX7MXHBWq78ni6/YynQce22Qi8I1ZGyNZ13hsz0MIq6QbZIlbVNPxuFQqGIL4o2oIrwhgTKRhjfdDytIytzv0kJ1ZEq0/3WOucfdI+I0BIzlQ77V9Cycx7DsQ1hdJiedEdYwHMn0ofyUlpwRqjaPI4q06O5RodoMh/SMvYChmiDqvYpXrMpLW7XuBF2eN2C/J2PcU7b0j3lHa7VbqIbyHsMstCO6lOTnuoqZiPZRnXn6bzPdfRV/DtWwHWrLdOs309T7Z0/zjk16Z85VtcTFQpFHuVlCatVEwUkoaN8alfnwvBnHCXUmFJcM0hqPLyBYepWUqLbGUTVlpCcRku5bawDBjFjhcBQoOJ8PIiqQr8WgrQGV4TtrP9JOW8Fz0OZJpTwSlACG8l9R1MaguXsI5RMp1Hiw/MxbVxMSbUbJShTN2LEYu10IqRQueZ+RtmBJAZVaR/Zt5xlsf94SrJNed4xbB8kNKhNkaljFq+1mXVA4oW6dmui2gAyxFf5bP4UujmGRP8dfT1v4PP4QT8VhUKhyHyOXIJSUyolsULO8UomTVSceuBScm8C5eDcfjElxpq0dL2Tlq7P0RCmCS0yS1vn9WQbj7P2GavYp2m80tNysyjDZMQ/2+4NNGxJdaVpullMoEtLeWt/AUq/ddiuWk4YO1iunmP9T6HrRS+G+3uSlrkn0hDpSUrNKUy6nOoLsKBQKBQKv+SYVRd9hCrLZvx/Fh35y9AVo2aC9dQNS+RLNWQZ/gaj+cr45nFfBcZKPcvK27ifalfjXtKe+5tb5w02+6jafNtijkMZ5Wcsw8QV5/4uPMeNjnMN97e09t0WK7AAM3KstycUtHCdTnVrKi1K72DAhEuoAu5HFe9VdkB1hUKhUOQc5lje+d+aORrL0pdviXWsDB3UZ3CdbgN9KW+ku8j0kGt8YjMAxBMlU6hk7etLZpNCCe0OJmY2TPtMOwoOJd4Pua8m/QFTKbFdyugzha1kyJ15nskLmeK08Vm6lhS3JGbc36wYzw7tnBByrCDXXU30nGGUTiczss1ASs3IgPKzdn+FQqFIjDlmifFFJBLZ4PyHZeYVQXTtD2uHHc0AL8dgETpEaFwQdcswVp2wRkW4uZQQ5guDmCutYAIm4LitToTFKdSViL86J4iuC4IxzWc6LVieIl/lNlrMwlUE7hXIzQgLUETxwVoh/AbhvrJV9sH1ARawcIO4kcHLERj8J2utFOuKdeQn3FeWWa4e1wj1E4Izfo2Qxwfpc2nIc4W7xnquc8LV4zu2tSHbAEterCFeFETXTBUKhUKRAApk47UhnSE7xflCU4QGCpng4mVkwH8shLsj1ms+MARr94VkADCKgdQJo5fZQdSwB36YMASqAiYm5/1Cq8+uQkh8DIMaGKPAAvVQMkhkFOkQRP0qD5hZBFHjFjA3MPF1tDaFVNYYjJ1BvuG/eCzTVFXjRABMDoY8bxvpmfVvYfVICO1L6Hy6Ocdqx6WyKS3Xw73Wlu0iMlpY/cJ/sgTrnWBNTM7X7q5QKBQ5X4StRiOZ8lSJjrWOYQ2vZ8h5cNEo6+yDuvJqJuXdayLJUN35DtWL7aiGrMV1whfoBrKWbhtvUS25x4qsM4SRZjZRhTqP0Wz6MxJPKlWYE2lw9AdVmE8yvVMNuo0cTjeVl7lOeTKd7+37Xe8xVCrKa9d19r/OhNCn8n5rMJdldfo2KhQKhSJtPCnr1xxjNOYwhjpLtWOr0phkny8tEplPK+v/ybRALUqfRfhQfkDGUYTriA/TgGUtj91DxlnBqbs7DXAGktFt4Zrkczxukh//RP/D3xl39GkyqGKwCmU9wy1mOpUBv9uTgVVmu8px3bUP/R+vdNpzC31Ii8R4hiWs33WZkeNs7eoKhUJx8DLH2WQyPZ39l4RZWNKdYxmtU4uRIdSgoU0FWmrezbiwkLom0c2hfixneBrWwDjoc4Rco/GPMerpxDJ1+B/S5Us07CnC9FLrQgJ8F6Glax+m8NrIQOkzGFP2fjLBzoylehuZ7QwyxgYJPMeqdJFBrsWlDCRQSbu7QqFQpI85RrK5MVjf+8UNIEC1KdYDYWxSyXMe1tcQgxRh1YxxC6QrxDadT5oLkvN3J9iWy2Vzi9Azco5ZF8Q6KJz+4UiPAAFYE0QIu9lS5jTrXMQexVRjs+wfmMC1sMZZn+1HYALknDSJjWHgg7B1MBgaJ/XtDKkDBkNDhd5kHQiIgFB3cEuZyfu4U7u8QqFQJMYcD5aGIrPGrjhlylBNWc5OeJzO673Jaz5LSfQIOvf/yKTD++lPuJ8q38rWuVDDvmiyjGTBsynPNUdjVWuehQlh1xLBALS7KxQKRfokx5zc0EJM8Fsmi643nvFV+zCqzN7U2PiEhjmf0KBnTha2dSwZdHtrH9S6z2gXVygUilzMHLPhwaQwas8vlARN8POXKKUhgPd3DCJuGCe23zOIevUsamc9RuNp5+yvmVXMWaFQKJQ55o2HUoWuHOPpmmHwGDN8IGFwKRr1bKMF61aWeY9GMCNMho1Mbms3E9FHoVAoFJnDHPPl0YdQkpanLeDmEUQzgZQgwVEfDvTICIJIPsiI8U4kEvlTCIY5cNaH8Q+CAcBoCEZFiOSzCufAtSQz2y5tgKHN59qVFQqFIvNQILffIIOKI2IOwqnBLQJGPshy3yKIhlaDxSei9SBNE5Iow/IUUW0Q4Pw28KMgmmrLMKd36ZSPCDvThMCoEI3nYiFY3c6U40/IdqCU3aJdTKFQKBQ5jTGWpqEM/CnPZsAA+BKOYxQbNwhAXfoXTmAaqfW0UM3vlOtOg5gZNMgpYx07gv6Ss7heOYQO/4X1jSgUCkWO5Rc5x88xi24Y/n+IRYoA5kgejEDj8B/c7JQDA/wiiPpHzpfjcNBfF0SDj08WWiOEPI8Ico4g34iL+nsQVb+Wl/KdHGkVyZNTKHWmUHqFynaqlH1Zu6JCoVDkLOZoIy8wx+rCjJYnUA4qVKhZqwqdL+fsZYSe3mSCcM6H8z58CcFI3xEC84QDP1S1D8s5E5w6j5XN5WSmWLuEb2RzKfeDdkWFQqFQ5pjTHwrWGBFV5z2hd4V5jeN+qF0HBNGoM1inXE7GCGkSOShhiIOMIHC43y5US879PeQaJYNoKqlFbgovhUKhUOQs5qiIPpR76Nc4Mr3MlXXU1KepUCgUBydztOn/BBgAMRFYrgRZm/QAAAAASUVORK5CYII=")
                elAttr "img" attr5 $ pure ()
                divClass "au-header__text" $ do
                  elClass "h1" "au-header__heading" $ do
                    text "Example Site"
                  divClass "au-header__subline" $ do
                    text "An example of the system in use"
      elClass "nav" "au-main-nav au-main-nav--dark" $ do
        divClass "container-fluid" $ do
          divClass "row" $ do
            divClass "col-md-12" $ do
              let
                attr6 = ("class" =: "au-main-nav__content") <>
                        ("id" =: "main-nav-default-alt")
              elAttr "div" attr6 $ do
                let
                  attr7 = ("aria-controls" =: "main-nav-default-alt") <>
                          ("class" =: "au-main-nav__toggle au-main-nav__toggle--open") <>
                          ("onClick" =: "return AU.mainNav.Toggle( this )")
                (btnEl8,_) <- elAttr' "button" attr7 $ do
                  text "Menu"
                  return ()
                {-
                  let
                    btnEv8 = domEvent Click btnEl8
                -}
                divClass "au-main-nav__menu" $ do
                  divClass "au-main-nav__menu-inner" $ do
                    divClass "au-main-nav__focus-trap-top" $ return ()
                    let
                      attr9 = ("aria-controls" =: "main-nav-default-alt") <>
                              ("class" =: "au-main-nav__toggle au-main-nav__toggle--close") <>
                              ("onClick" =: "return AU.mainNav.Toggle( this )")
                    (btnEl10,_) <- elAttr' "button" attr9 $ do
                      text "Close"
                      return ()
                    {-
                      let
                        btnEv10 = domEvent Click btnEl10
                    -}
                    elClass "ul" "au-link-list" $ do
                      el "li" $ do
                        let
                          attr11 = ("href" =: "#")
                        elAttr "a" attr11 $ do
                          text "About"
                          return ()
                        return ()
                      elClass "li" "active" $ do
                        let
                          attr12 = ("href" =: "#")
                        elAttr "a" attr12 $ do
                          text "Responsibilities"
                          return ()
                        return ()
                      el "li" $ do
                        let
                          attr13 = ("href" =: "#")
                        elAttr "a" attr13 $ do
                          text "Templates"
                          return ()
                        return ()
                      el "li" $ do
                        let
                          attr14 = ("href" =: "#")
                        elAttr "a" attr14 $ do
                          text "Community"
                          return ()
                        return ()
                      el "li" $ do
                        let
                          attr15 = ("href" =: "#")
                        elAttr "a" attr15 $ do
                          text "Support"
                          return ()
                        return ()
                      return ()
                    divClass "au-main-nav__focus-trap-bottom" $ return ()
                    return ()
                  return ()
                let
                  attr16 = ("aria-controls" =: "main-nav-default-alt") <>
                          ("class" =: "au-main-nav__overlay") <>
                          ("onClick" =: "return AU.mainNav.Toggle( this )")
                elAttr "div" attr16 $ return ()
                return ()
              return ()
            return ()
          return ()
        return ()
      divClass "banner au-body au-body--alt" $ do
        divClass "container-fluid" $ do
          divClass "row" $ do
            divClass "col-sm-6" $ do
              el "h3" $ do
                text "Understand user needs. Research to develop a deep knowledge of the users and their context for using the service."
                return ()
              el "br" $ return ()
              let
                attr17 = ("class" =: "au-btn au-btn--secondary")
              (btnEl18,_) <- elAttr' "button" attr17 $ do
                text "Examples"
                return ()
              {-
                let
                  btnEv18 = domEvent Click btnEl18
              -}
              let
                attr19 = ("class" =: "au-btn")
              (btnEl20,_) <- elAttr' "button" attr19 $ do
                text "Get Started"
                return ()
              {-
                let
                  btnEv20 = domEvent Click btnEl20
              -}
              return ()
            divClass "col-sm-6" $ do
              let
                attr21 = ("alt" =: "") <>
                        ("class" =: "au-responsive-media-img banner__image") <>
                        ("src" =: "http://placehold.it/900x300")
              elAttr "img" attr21 $ return ()
              return ()
            return ()
          return ()
        return ()
      divClass "container-fluid au-body main-content" $ do
        divClass "row" $ do
          divClass "col-md-12" $ do
            let
              attr22 = ("aria-label" =: "breadcrumb") <>
                      ("class" =: "au-breadcrumbs")
            elAttr "nav" attr22 $ do
              elClass "ul" "au-link-list au-link-list--inline" $ do
                el "li" $ do
                  let
                    attr23 = ("href" =: "#")
                  elAttr "a" attr23 $ do
                    text "Home"
                    return ()
                  return ()
                el "li" $ do
                  let
                    attr24 = ("href" =: "#")
                  elAttr "a" attr24 $ do
                    text "Responsibilities"
                    return ()
                  return ()
                el "li" $ do
                  let
                    attr25 = ("href" =: "#")
                  elAttr "a" attr25 $ do
                    text "User needs"
                    return ()
                  return ()
                el "li" $ do
                  let
                    attr26 = ("href" =: "#")
                  elAttr "a" attr26 $ do
                    text "Understand user needs"
                    return ()
                  return ()
                el "li" $ do
                  text "Community"
                  return ()
                return ()
              return ()
            return ()
          return ()
        divClass "row" $ do
          divClass "col-sm-4 col-md-3" $ do
            let
              attr27 = ("class" =: "au-side-nav au-accordion") <> ("id" =: "nav")
            elAttr "aside" attr27 $ do
              let
                attr28 = ("aria-controls" =: "nav-default") <>
                        ("aria-expanded" =: "false") <> ("aria-selected" =: "false") <>
                        ("class" =: "au-side-nav__toggle au-accordion__title au-accordion--closed")
                        <> ("href" =: "#nav-default") <>
                        ("onclick" =: "return AU.accordion.Toggle( this )") <>
                        ("role" =: "tab")
              elAttr "a" attr28 $ do
                text "In this section"
                return ()
              let
                attr29 = ("aria-hidden" =: "true") <>
                        ("class" =: "au-side-nav__content au-accordion--closed au-accordion__body")
                        <> ("id" =: "nav-default")
              elAttr "div" attr29 $ do
                elClass "h2" "au-sidenav__title" $ do
                  let
                    attr30 = ("href" =: "#")
                  elAttr "a" attr30 $ do
                    text "Responsibilities"
                    return ()
                  return ()
                elClass "ul" "au-link-list" $ do
                  el "li" $ do
                    let
                      attr31 = ("href" =: "#")
                    elAttr "a" attr31 $ do
                      text "Side-nav item level 1"
                      return ()
                    return ()
                  el "li" $ do
                    let
                      attr32 = ("href" =: "#")
                    elAttr "a" attr32 $ do
                      text "User Needs"
                      return ()
                    elClass "ul" "au-link-list" $ do
                      el "li" $ do
                        let
                          attr33 = ("href" =: "#")
                        elAttr "a" attr33 $ do
                          text "Understand user needs"
                          return ()
                        elClass "ul" "au-link-list" $ do
                          elClass "li" "active" $ do
                            let
                              attr34 = ("href" =: "#")
                            elAttr "a" attr34 $ do
                              text "Community"
                              return ()
                            return ()
                          return ()
                        return ()
                      return ()
                    return ()
                  el "li" $ do
                    let
                      attr35 = ("href" =: "#")
                    elAttr "a" attr35 $ do
                      text "Side-nav item level 1"
                      return ()
                    return ()
                  return ()
                return ()
              return ()
            return ()
          divClass "col-sm-8 col-md-8 col-md-offset-1" $ do
            let
              attr36 = ("id" =: "content")
            elAttr "main" attr36 $ do
              el "h3" $ do
                text "Community"
                return ()
              el "p" $ do
                text "Together we'll be able to build products and services better, faster and easier"
                return ()
              el "p" $ do
                text "Join in the community today. This is a great chance for you to:"
                return ()
              el "ul" $ do
                el "li" $ do
                  text "collaborate on developing up great solutions to common design problems"
                  return ()
                el "li" $ do
                  text "share your hard work, insights and learnings with others"
                  return ()
                el "li" $ do
                  text "get help with your own design and development challenges"
                  return ()
                return ()
              return ()
            return ()
          return ()
        return ()
      let
        attr37 = ("class" =: "au-footer au-footer--dark") <>
                ("role" =: "contentinfo")
      elAttr "footer" attr37 $ do
        divClass "container-fluid" $ return ()
        elClass "nav" "au-footer__navigation row au-body au-body--dark" $ do
          divClass "col-md-4 col-sm-6" $ do
            elClass "ul" "au-link-list au-link-list--inline" $ do
              el "li" $ do
                let
                  attr38 = ("href" =: "#")
                elAttr "a" attr38 $ do
                  text "Privacy"
                  return ()
                return ()
              el "li" $ do
                let
                  attr39 = ("href" =: "#")
                elAttr "a" attr39 $ do
                  text "Need Help?"
                  return ()
                return ()
              el "li" $ do
                let
                  attr40 = ("href" =: "#")
                elAttr "a" attr40 $ do
                  text "Accessibility"
                  return ()
                return ()
              return ()
            return ()
          return ()
        elClass "section" "au-footer__end row" $ do
          divClass "col-sm-12" $ do
            el "p" $ do
              el "small" $ do
                text "© Commonwealth of Australia"
                return ()
              return ()
            return ()
          return ()
        return ()

main :: IO ()
main = mainWidgetWithHead (fst frontend) (snd frontend)
