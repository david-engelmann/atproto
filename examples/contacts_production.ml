(* Offline sketch: wire production phone / contacts / push clients
   against hosted Bluesky AppView + a caller-supplied push gateway.
   This is not an SMS gateway and not an APNs/FCM product. Official
   @atproto/dev-env 0.6.4 TestNetwork does not start either. No
   network is used here.

   Contacts (app.bsky.contact.* lexicons):

     1. Password session: Contact.get_matches / get_sync_status /
        import_contacts through the PDS / entryway.
     2. OAuth DPoP: mint getServiceAuth (aud = AppView DID, lxm =
        the app.bsky.contact.* NSID) and call *_service on
        Client.appview_host_from_env. DPoP cannot be the AppView
        Bearer.
     3. Hosted SMS: start_phone_verification → verify_phone (token)
        → import_contacts. Live hops need ATP_PHONE +
        ATP_PHONE_NUMBER. This sketch only builds the JSON.

   Signup SMS is a different NSID: Temp.request_phone_verification
   (com.atproto.temp.requestPhoneVerification). Privileged hosted
   PDS; also not faked. Server.describe_server reports
   phoneVerificationRequired; createAccount accepts
   verificationPhone / verificationCode.

   Push (app.bsky.notification.registerPush / unregisterPush):

     1. Caller supplies serviceDid + device token + platform + appId.
     2. Official Bluesky push is closed to the official app.
        Xrpc.notif_proxy is the public #bsky_notif fragment, not a
        default credential.
     3. Optional atproto-proxy from ~proxy or ATP_PUSH_DID
        (unregisterPush may need it). Live hops need ATP_PUSH.
*)

open Atproto.Contact
open Atproto.Notification
open Atproto.Temp
open Atproto.Server
open Atproto.Xrpc
open Atproto.Client

let () =
  assert (
    Xrpc.proxy_to_string Xrpc.notif_proxy = "did:web:api.bsky.app#bsky_notif");
  assert (Xrpc.notif_proxy.service = "bsky_notif");
  assert (Notification.platform_ios = "ios");
  assert (Notification.platform_android = "android");
  assert (Notification.platform_web = "web");
  assert (Notification.effective_push_proxy () = None);
  let gateway =
    { Xrpc.did = "did:web:push.example.org"; service = "bsky_notif" }
  in
  assert (
    Notification.effective_push_proxy ~proxy:gateway () = Some gateway);
  assert (
    Notification.push_proxy_headers ~proxy:gateway ()
    = [ Xrpc.proxy_header gateway ]);
  assert (Notification.push_proxy_headers () = []);
  let matches = Contact.get_matches_body ~limit:25 ~cursor:"c1" () in
  assert (List.assoc "limit" matches = "25");
  assert (List.assoc "cursor" matches = "c1");
  assert (Contact.get_matches_body () = []);
  let start = Contact.start_phone_verification_body ~phone:"+12125550123" in
  let verify =
    Contact.verify_phone_body ~phone:"+12125550123" ~code:"123456"
  in
  let import =
    Contact.import_contacts_body ~token:"jwt" ~contacts:[ "+12125550124" ]
  in
  let dismiss =
    Contact.dismiss_match_body ~subject:"did:plc:abc123xyz0001112223333"
  in
  let notify =
    Contact.send_notification_body ~from:"did:plc:from" ~to_:"did:plc:to"
  in
  let open Yojson.Safe.Util in
  assert (start |> member "phone" |> to_string = "+12125550123");
  assert (verify |> member "code" |> to_string = "123456");
  assert (import |> member "token" |> to_string = "jwt");
  assert (
    dismiss |> member "subject" |> to_string = "did:plc:abc123xyz0001112223333");
  assert (notify |> member "from" |> to_string = "did:plc:from");
  (match Contact.remove_data_body with `Assoc [] -> () | _ -> assert false);
  let signup =
    Temp.request_phone_verification_body ~phone_number:"+15555550100" ()
  in
  assert (signup |> member "phoneNumber" |> to_string = "+15555550100");
  let account =
    Server.create_account_body ~handle:"alice.test" ~email:"a@test.com"
      ~password:"secret" ~verification_phone:"+15555550100"
      ~verification_code:"000000" ()
  in
  assert (account |> member "verificationPhone" |> to_string = "+15555550100");
  assert (account |> member "verificationCode" |> to_string = "000000");
  let desc =
    Server.parse_describe_server
      (`Assoc
        [
          ("did", `String "did:web:bsky.social");
          ("availableUserDomains", `List [ `String ".bsky.social" ]);
          ("phoneVerificationRequired", `Bool true);
        ])
  in
  assert (desc.phone_verification_required = Some true);
  assert (Notification.list_activity_subscriptions_body () = []);
  assert (
    Notification.list_activity_subscriptions_body ~limit:10 ~cursor:"a1" ()
    = [ ("limit", "10"); ("cursor", "a1") ]);
  let reg =
    Notification.register_push_body ~service_did:"did:web:push.example.org"
      ~token:"device-token" ~platform:Notification.platform_ios
      ~app_id:"org.example.app" ~age_restricted:false ()
  in
  assert (reg |> member "serviceDid" |> to_string = "did:web:push.example.org");
  assert (reg |> member "platform" |> to_string = "ios");
  assert (reg |> member "ageRestricted" |> to_bool = false);
  let unreg =
    Notification.unregister_push_body ~service_did:"did:web:push.example.org"
      ~token:"device-token" ~platform:Notification.platform_android
      ~app_id:"org.example.app" ()
  in
  assert (unreg |> member "platform" |> to_string = "android");
  ignore Contact.get_matches;
  ignore Contact.get_matches_appview;
  ignore Contact.get_matches_service;
  ignore Contact.get_sync_status;
  ignore Contact.get_sync_status_appview;
  ignore Contact.get_sync_status_service;
  ignore Contact.import_contacts;
  ignore Contact.import_contacts_service;
  ignore Contact.start_phone_verification;
  ignore Contact.start_phone_verification_service;
  ignore Contact.verify_phone;
  ignore Contact.verify_phone_service;
  ignore Notification.register_push;
  ignore Notification.unregister_push;
  ignore Temp.request_phone_verification;
  ignore Client.appview_host_from_env;
  (* Skip gates stay off unless the operator opts in. This sketch
     never sends SMS or registers a device. *)
  ignore Contact.phone_live_enabled;
  ignore Contact.phone_number_from_env;
  ignore Notification.push_live_enabled
