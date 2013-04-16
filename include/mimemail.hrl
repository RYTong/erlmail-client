-record(mimemail, {type,
                   subtype,
                   headers,
                   properties,
                   body       %% binary() | mimemail() | [mimemail()]
                  }).