open Format

let enclave_status_var_str = 
	" sgx_enclave_id_t   eid;
 sgx_status_t       ret   = SGX_SUCCESS;
 sgx_launch_token_t token = {0};
 int updated = 0;
	"
let load_enclave_str oc eid = 
let _ = Printf.fprintf oc " // Create the Enclave with above launch token\n" in
let _ = Printf.fprintf oc " ret = sgx_create_enclave(ENCLAVE%s_FILE, SGX_DEBUG_FLAG, &token, &updated, &eid, NULL);\n" (string_of_int eid) in
let _ = Printf.fprintf oc " if (ret != SGX_SUCCESS) { \n    printf(\"App: error %%x, failed to create enclave.\", ret);\n    return -1;\n }\n" in
 ()

let destroy_enclave_str oc  eid = 
	let _ = Printf.fprintf oc " // Destroy the enclave when all Enclave calls finished.\n" in
		Printf.fprintf oc " if(SGX_SUCCESS != sgx_destroy_enclave(eid)) \n return -1; \nreturn 0;\n"

