

let enclave_status_var_str = 
	" sgx_enclave_id_t   eid;
 sgx_status_t       ret   = SGX_SUCCESS;
 sgx_launch_token_t token = {0};
 int updated = 0;
	"
let load_enclave_str = 
" // Create the Enclave with above launch token
 ret = sgx_ create_ enclave(ENCLAVE_ FILE, SGX_ DEBUG_ FLAG, &token, &updated, &eid, NULL);
 if (ret != SGX_SUCCESS) {
	printf(\"App: error %#x, failed to create enclave.\\n\", ret);
	return -1;
 }"
