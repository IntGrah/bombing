port module Discord exposing (Auth, authenticated)


port authenticated : (Auth -> msg) -> Sub msg


type alias Auth =
    { access_token : String
    , user :
        { username : String
        , discriminator : String
        , id : String

        -- , public_flags : Int
        , avatar : Maybe String
        , global_name : Maybe String
        }
    , -- scopes: Scopes
      expires : String

    -- , application :
    --     { id : String
    --     , description : String
    --     , name : String
    --     , icon : Maybe String
    --     , rpc_origins : Maybe (List String)
    --     }
    }



---- type Scopes = (-1 | "identify" | "email" | "connections" | "guilds" | "guilds.join" | "guilds.members.read" | "guilds.channels.read" | "gdm.join" | "bot" | "rpc" | "rpc.notifications.read" | "rpc.voice.read" | "rpc.voice.write" | "rpc.video.read" | "rpc.video.write" | "rpc.screenshare.read" | "rpc.screenshare.write" | "rpc.activities.write" | "webhook.incoming" | "messages.read" | "applications.builds.upload" | "applications.builds.read" | "applications.commands" | "applications.commands.permissions.update" | "applications.commands.update" | "applications.store.update" | "applications.entitlements" | "activities.read" | "activities.write" | "relationships.read" | "relationships.write" | "voice" | "dm_channels.read" | "role_connections.write" | "presences.read" | "presences.write" | "openid" | "dm_channels.messages.read" | "dm_channels.messages.write" | "gateway.connect" | "account.global_name.update" | "payment_sources.country_code" | "sdk.social_layer")[]
