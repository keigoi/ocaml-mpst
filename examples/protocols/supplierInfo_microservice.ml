open Mpst
open Mpst.Types
open Usecase_util


(* aux global protocol FilterInfo<sig Query>
 * (role authorisersvc, role filtersvc) {
 * Query connect authorisersvc to filtersvc;
 * filtered() from filtersvc to authorisersvc;
 * disconnect authorisersvc and filtersvc;
 * } *)
  let filterinfo cont =
    (authorisesvc --> filtersvc) query @@
      (filtersvc --> authorisesvc) filtered @@
        cont

(* aux global protocol SuppInfo (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc
 * ) {
 * choice at authorisersvc {
 * // DENIED
 * deny() from authorisersvc to requestor;
 * } or {
 * connect authorisersvc to suppliersvc;
 * // PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR
 * getsuppliers() from authorisersvc to suppliersvc;
 * suppliers() from suppliersvc to authorisersvc;
 * do FilterInfo
 * <filterSuppliers(usercontext, filters, supplierdetails)>
 * //<filterContracts(usercontext, filters, supplierdetails)>
 * (authorisersvc, filtersvc);
 * disconnect authorisersvc and suppliersvc;
 * suppliers() from authorisersvc to requestor;
 * }
 * } *)
  (* XXX non-directed chocie *)
  let to_requestor_or_suppliersvc =
    {disj_concat=(fun l r -> object method role_Request=l#role_Request method role_Supply=r#role_Supply end);
     disj_splitL=(fun lr -> (lr :> <role_Request : _>));
     disj_splitR=(fun lr -> (lr :> <role_Supply : _>))}

  let suppinfo () =
    choice_at authorisesvc (to_requestor_or_suppliersvc) (* uses nondirected output *)
      (authorisesvc, (authorisesvc --> requestor) deny @@
                       (* dummy sending due to lack of explicit connection handling  *)
                       (authorisesvc --> suppliersvc) dummy @@
                         (authorisesvc --> filtersvc) dummy @@
                           finish)
      (authorisesvc, (authorisesvc --> suppliersvc) getsuppliers @@
                       (suppliersvc --> authorisesvc) suppliers @@
                         filterinfo @@
                           (authorisesvc --> requestor) suppliers @@
                             finish)


(* aux global protocol ContractInfo (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role contractsvc
 * ) {
 * choice at authorisersvc {
 * // DENIED
 * deny() from authorisersvc to requestor;
 * } or {
 * connect authorisersvc to contractsvc;
 * // PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR
 * getcontracts() from authorisersvc to contractsvc;
 * contracts() from contractsvc to authorisersvc;
 * do FilterInfo
 * <filterContracts(usercontext, filters, contractdetails)>
 * (authorisersvc, filtersvc);
 * disconnect authorisersvc and contractsvc;
 * contracts() from authorisersvc to requestor;
 * }
 * } *)

  (* XXX non-directed chocie *)
  let to_requestor_or_contractsvc =
    {disj_concat=(fun l r -> object method role_Request=l#role_Request method role_Contract=r#role_Contract end);
     disj_splitL=(fun lr -> (lr :> <role_Request : _>));
     disj_splitR=(fun lr -> (lr :> <role_Contract : _>))}

  let contractinfo () =
    choice_at authorisesvc to_requestor_or_contractsvc
      (authorisesvc, (authorisesvc --> requestor) deny @@
                       (* dummy sending due to lack of explicit connection handling  *)
                       (authorisesvc --> contractsvc) dummy @@
                         (authorisesvc --> filtersvc) dummy @@
                           finish)
      (authorisesvc, (authorisesvc --> contractsvc) getcontracts @@
                       (contractsvc --> authorisesvc) contracts @@
                           filterinfo @@
                             (authorisesvc --> requestor) contracts @@ finish)

(* aux global protocol Main (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc,
 * role contractsvc
 * ) {
 * choice at requestor {
 * // GET SUPPLIER INFO
 * getsuppliers(uuid) from requestor to authorisersvc;
 * do SuppInfo(requestor, authorisersvc, filtersvc, suppliersvc);
 * } or {
 * // GET CONTRACT INFO
 * getcontracts() from requestor to authorisersvc;
 * do ContractInfo(requestor, authorisersvc, filtersvc, contractsvc);
 * }
 * do Main(requestor, authorisersvc, filtersvc, suppliersvc, contractsvc);
 * } *)

  let main () =
    choice_at requestor (to_authorisesvc getsuppliers_or_getcontracts)
      (requestor, (requestor --> authorisesvc) getsuppliers @@
                    (authorisesvc --> contractsvc) dummy @@
                      suppinfo ())
      (requestor, (requestor --> authorisesvc) getcontracts @@
                    (authorisesvc --> suppliersvc) dummy @@
                      contractinfo ())

(* explicit global protocol PartnershipSupplier (
 * role loginsvc,
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc,
 * role contractsvc
 * ) {
 * connect requestor to loginsvc;
 * login(username, password) from requestor to loginsvc;
 * choice at loginsvc {
 * loginfailure() from loginsvc to requestor;
 * disconnect requestor and loginsvc;
 * } or {
 * loginsuccess() from loginsvc to requestor;
 * connect requestor to authorisersvc;
 * do Main(requestor, authorisersvc, filtersvc, suppliersvc, contractsvc);
 * }
 * } *)

  let partnership_supplier () =
    (requestor --> loginsvc) login @@
      choice_at loginsvc (to_requestor loginfailure_or_loginsuccess)
        (loginsvc, (loginsvc --> requestor) loginfailure @@
                     (requestor --> authorisesvc) dummy @@
                       (authorisesvc --> contractsvc) dummy @@
                         (authorisesvc --> suppliersvc) dummy @@
                           (authorisesvc --> filtersvc) dummy @@
                             finish)
        (loginsvc, (loginsvc --> requestor) loginsuccess @@
                     main ())
