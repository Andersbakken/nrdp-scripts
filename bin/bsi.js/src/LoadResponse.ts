export interface LoadResponse {
    url: string;
    status: number;
    json(): Promise<unknown>;
    text(): Promise<string>;
}
